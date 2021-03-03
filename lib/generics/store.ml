open Series.Util

module KeyPath = struct
  type key =
    | Name of string
    | Indice of int

  type path =
    | FromRoot of key list     (* In path order:    /a/b/c -> [a;b;c] *)
    | FromLeaf of key list     (* In reverse order: /a/b/c -> [c;b;a] *)

  let string_of_key = function
    | Name s -> s
    | Indice i -> string_of_int i

  let key_of_string k =
    Name k

  let root =
    FromRoot []

  let from_root = function
    | FromRoot p -> p
    | FromLeaf r -> List.rev r

  let from_leaf = function
    | FromRoot p -> List.rev p
    | FromLeaf r -> r

  let add_suffix path suffix =
    FromLeaf ((Name suffix) :: (from_leaf path))

  let first_indice path =
    FromLeaf ((Indice 0) :: (from_leaf path))

  let next_indice path =
    match from_leaf path with
    | (Indice n) :: r ->
      FromLeaf ((Indice (n+1)) :: r)
    | r ->
      FromLeaf ((Indice 0) :: r)

  let extract_suffix prefix =
    let rec loop prefix xs =
      match prefix, xs with
      | [], suffix -> Some (FromRoot suffix)
      | p::prefix, x::xs when p = x -> loop prefix xs
      | _ -> None
    in
    fun xs ->
      loop (from_root prefix) (from_root xs)
end

module type KVStore = sig
  type t
  type key = KeyPath.key
  type path = KeyPath.path
  type value = string

  exception Unknown_path of path
  exception Unknown_key of key
  exception Read_error of string
  exception Write_error of string

  (** The set of paths mapped to some value. *)
  val paths: t -> path Series.Bounded.producer

  (** The set of (path, value) pairs. *)
  val all_pairs: t -> (path * value) Series.Bounded.producer

  (** Get the value mapped to the given path (if any). *)
  val get_path: t -> path -> value option

  (** Set the value mapped to the given path

      Remove the path if no value is provided. *)
  val set_path: t -> path -> value option -> t

  (** The set of keys mapped to some value.

      This is the subset of paths having a length of one. *)
  val keys: t -> key Series.Bounded.producer

  (** The set of (key, value) pairs.

      This is the subset of all_pairs having a path of length one. *)
  val pairs: t -> (key * value) Series.Bounded.producer

  (** Get the value mapped to the given key (if any). *)
  val get: t -> key -> value option

  (** Set the value mapped to the given key

      Remove the key if no value is provided. *)
  val set: t -> key -> value option -> t

  (** Change root.

    [focus prefix kv] is an extract of the former key-value store
    which contains only the pairs with the given prefix.

    This change of the root impacts all the read and write operations
    which are than to be understood as relative to the new root,
    with no impact on the (path, value) pair having a different prefix.

    * [ paths (focus prefix kv) = paths kv |> filter_map (extract_suffix prefix) ]
    * [ all_pairs (focus prefix kv) = all_pairs kv |> filter_map_fst (extract_suffix prefix) ]
    * [ keys (focus prefix kv) = paths kv |> filter_map (extract_suffix prefix) |> filter_map length_one ]
    * [ pairs (focus prefix kv) = all_pairs kv |> filter_map_fst (extract_suffix prefix) |> filter_map_fst length_one ]
    * [ get (focus prefix kv) k = get kv (prefix @@ k) ] 
    * [ set (focus prefix kv) k ov = focus prefix (set kv (prefix @@ k) ov) ]

    The [focus] and [root] functions are absolute,
    so we can restore the focus on a more globale key value store.

    * [ focus p2 (focus p1 kv) = focus p2 kv ]
    * [ root (focus prefix kv) = prefix ] *)
  val focus: path -> t -> t

  (** The root used as prefix for all paths for all operations. *)
  val root: path
end

module KVMapping (KV : KVStore) = struct

  module Mapping = Series.Mapping
  module Seq = Series.Bounded

  let wrap_kvstore encode_key decode_key encode_val decode_val =
    let encode_key = encode_key >> KeyPath.key_of_string in
    let decode_key = KeyPath.string_of_key >> decode_key in
    let decode_pair (k,v) = (decode_key k, decode_val v) in
    let keys =
      KV.keys >> Seq.map decode_key
    in
    let pairs =
      KV.pairs >> Seq.map decode_pair
    in
    let value kv =
      encode_key >> KV.get kv >> Option.map decode_val
    in
    let apply_update u kv = match u with
      | Mapping.Replace (k,v) -> KV.set kv (encode_key k) (Some (encode_val v))
      | Mapping.Remove k      -> KV.set kv (encode_key k) None
    in
    let rec wrap kv =
      let update =
        Seq.fold apply_update kv >> wrap
      in
      {
        Mapping.keys = keys kv;
        Mapping.pairs = pairs kv;
        Mapping.value = value kv;
        Mapping.update;
      }
    in
    wrap >> Mapping.of_source
end

module GenKVStore (KV : KVStore) (Fmt : Serialization.Encoding)
  : Interpretation.S
= struct
  type 'a layout = {
    read_value: KV.t -> 'a;
    write_value: KV.t -> 'a -> KV.t;
  }

  type 'a tc = {
    layout: KV.path -> 'a layout;
    format: 'a Fmt.tc
  }

  let _store tc = tc.layout KV.root
  
  let get_value path kv = 
    KV.get_path kv path
    |> Option.if_none (fun () -> raise (KV.Unknown_path path))

  let update_value path kv v =
     KV.set_path kv path (Some v)

  let raise_read_error r =
    Result.on_error (fun err -> raise (KV.Read_error err)) r

  let raise_write_error r =
    Result.on_error (fun err -> raise (KV.Write_error err)) r

  let get_encoded_value decode path kv =
    get_value path kv
    |> decode

  let update_encoded_value encode path kv v =
    encode v
    |> update_value path kv

  (* Persisting atomic values *)

  let string : string tc =
    let layout key = {
      read_value = get_value key;
      write_value = update_value key; }
    in
    { layout; format = Fmt.string; }

  let atomic_value format =
    let decode = Fmt.decode format >> raise_read_error in
    let encode = Fmt.encode format >> raise_write_error in
    let layout key = {
      read_value = get_encoded_value decode key;
      write_value = update_encoded_value encode key; }
    in
    { layout; format; }

  let int : int tc = atomic_value Fmt.int
  let int64 : int64 tc = atomic_value Fmt.int64
  let float : float tc = atomic_value Fmt.float
  let bool : bool tc = atomic_value Fmt.bool

  let unit : unit tc = 
    (* Nothing is stored for a unit value *)
    let layout _key = {
      read_value = (fun _ -> ());
      write_value = (fun kv () -> kv); }
    in
    { layout; format = Fmt.unit; }

  (* Persisting tuples *)

  let layout_tuple pair key =
    pair (KeyPath.first_indice key)

  let layout_product head tail head_key =
    let tail_key = KeyPath.next_indice head_key in
    let head_layout = head head_key in
    let tail_layout = tail tail_key in
    let read_value kv =
      (head_layout.read_value kv, tail_layout.read_value kv)
    in
    let write_value kv (a,b) =
      let kv = head_layout.write_value kv a in
      tail_layout.write_value kv b
    in
    {
      read_value;
      write_value;
    }

  let pair head tail = {
      layout = layout_product head.layout tail.layout;
      format = Fmt.pair head.format tail.format;
    }

  let tuple pair = {
      layout = layout_tuple pair.layout;
      format = Fmt.tuple pair.format;
    }

  (* Persisting records *)

  let empty_record = unit

  let field_layout name field record_path =
    field (KeyPath.add_suffix record_path name)

  let field name field = {
      layout = field_layout name field.layout;
      format = Fmt.field name field.format;
    }

  let (&) = pair

  let record rows = rows

  (* Persisting lists *)

  let list_layout _item_layout _path =
    let read_value _kv =
      Series.Bounded.empty  (* FIXME *)
    in
    let write_value kv _items =
      kv (* FIXME *)
    in
    {
      read_value;
      write_value;
    }

  let list items = {
    layout = list_layout items.layout;
    format = Fmt.list items.format;
  }

  (* Persisting mapping *)

  module KVMap = KVMapping(KV)

  let mapping_layout key_format val_format path =
    let decode_key = Fmt.decode key_format >> raise_read_error in
    let encode_key = Fmt.encode key_format >> raise_write_error in
    let decode_val = Fmt.decode val_format >> raise_read_error in
    let encode_val = Fmt.encode val_format >> raise_write_error in
    let read_value kv =
      let kvmap = KV.focus path kv in
      KVMap.wrap_kvstore encode_key decode_key encode_val decode_val kvmap
    in
    let write_value kv _pairs =
      kv (* FIXME *)
    in
    {
      read_value;
      write_value;
    }

  let mapping key value = {
    layout = mapping_layout key.format value.format;
    format = Fmt.mapping key.format value.format;
  }
end
