module Json = Yojson.Safe
open Serialization.Result

type json = Json.t

module Decoder = struct
  type 'a decoder = json -> ('a,string) result
  type 'a tc = 'a decoder

  let decode decoder s =
    try decoder (Json.from_string s) 
    with Yojson.Json_error e -> Error e

  let string = function
    | `String v -> Ok v
    | json -> Error (Format.sprintf "not a string value: %s" (Json.pretty_to_string json))

  let int = function
    | `Int v -> Ok v
    | `Intlit v
    | `String v -> try_apply int_of_string v
    | json -> Error (Format.sprintf "not an int value: %s" (Json.pretty_to_string json))

  let int64 = function
    | `Int v -> Ok (Int64.of_int v)
    | `Intlit v
    | `String v -> try_apply Int64.of_string v
    | json -> Error (Format.sprintf "not an int value: %s" (Json.pretty_to_string json))

  let float = function
    | `Float v -> Ok v
    | `String v -> try_apply float_of_string v
    | json -> Error (Format.sprintf "not a float value: %s" (Json.pretty_to_string json))

  let bool = function
    | `Bool v -> Ok v
    | `String v -> try_apply bool_of_string v
    | json -> Error (Format.sprintf "not a bool value: %s" (Json.pretty_to_string json))

  let unit = function
    | `Null -> Ok ()
    | json -> Error (Format.sprintf "not a null value: %s" (Json.pretty_to_string json))

  let list item_decoder = function
    | `List vs | `Tuple vs ->
       iter_apply item_decoder vs
       >>= fun items ->
       ok (Series.Bounded.of_list items)
    | json -> Error (Format.sprintf "not a list: %s" (Json.pretty_to_string json))

  let pair (type a) (type b) (head_decoder: a decoder) (tail_decoder: b decoder): ((a*b) decoder) = function
    | `List [head;tail] | `Tuple [head;tail] ->
      head_decoder head >>= fun h -> tail_decoder tail >>= fun t -> ok (h,t)
    | `List (head::tail) | `Tuple (head::tail) ->
      head_decoder head >>= fun h -> tail_decoder (`List tail) >>= fun t -> ok (h,t)
    | _ -> Error "Not a tuple"

  let tuple pair_decoder = pair_decoder

  let empty_record = function
    | `Assoc _ -> Ok ()
    | json -> Error (Format.sprintf "not an object: %s" (Json.pretty_to_string json))

  let field name decoder = function
    | `Assoc pairs as json -> (
      try List.assoc name pairs |> decoder
      with _ -> Error (Format.sprintf "no '%s' field in object: %s" name (Json.pretty_to_string json))
    )
    | json -> Error (Format.sprintf "not an object: %s" (Json.pretty_to_string json))

  let (&) fst_decoder snd_decoder r =
    fst_decoder r >>= fun h -> snd_decoder r >>= fun t -> Ok (h,t)

  let record fields_decoder = fields_decoder

  let variant name decoder = function
    | `Variant (n,Some v) when n = name -> decoder v
    | `Variant (_,_) as json -> Error (Format.sprintf "not a '%s' variant: %s" name (Json.pretty_to_string json))
    | json -> Error (Format.sprintf "not a variant value: %s" (Json.pretty_to_string json))

  let symbol name v = function
    | `Variant (n,None) when n = name -> Ok v
    | `Variant (_,_) as json -> Error (Format.sprintf "not a '%s' variant: %s" name (Json.pretty_to_string json))
    | json -> Error (Format.sprintf "not a variant value: %s" (Json.pretty_to_string json))

  let union named_decoders = function
    | `Variant (tag,Some v) -> (
      try
        let decode = List.assoc tag named_decoders in
        decode v
      with _ -> Error (Format.sprintf "unknow tag '%s': %s" tag (Json.pretty_to_string v))
    )
    | json -> Error (Format.sprintf "not an object: %s" (Json.pretty_to_string json))

  let mapping key_decoder val_decoder =
    let pair_decoder = pair key_decoder val_decoder in
    let mapping =
      let rec loop m = function
        | [] -> Ok (Series.Mapping.commit_updates m)
        | kv::kvs -> (
          match pair_decoder kv with
          | Ok kv -> loop (Series.Mapping.replace kv m) kvs
          | Error r -> Error r
        )
      in
      loop (Series.Mapping.empty)
    in
    function
    | `List pairs ->
       mapping pairs
    | `Assoc pairs ->
       pairs
       |> List.map (fun (k,v) -> `List [`String k; v])
       |> mapping
    | json -> Error (Format.sprintf "not a mapping: %s" (Json.pretty_to_string json))
end

module Encoder = struct
  type 'a encoder = 'a -> json
  type 'a tc = 'a encoder

  let encode encoder a =
    try Ok (Json.to_string (encoder a))
    with Yojson.Json_error e -> Error e

  let string s = `String s
  let int i = `Int i
  let int64 i = `Intlit (Int64.to_string i)
  let float f = `Float f
  let bool b = `Bool b
  let unit () = `Null

  let list item_encoder xs =
    `List (Series.Bounded.to_list (Series.Bounded.map item_encoder xs))

  let pair head_encoder tail_encoder (head,tail) =
    let head = head_encoder head in
    let tail = tail_encoder tail in
    match tail with
      | `Tuple tail -> `Tuple (head::tail)
      | tail -> `Tuple [head;tail]  
     
  let tuple pair_encoder = pair_encoder

  let empty_record () = `Assoc []

  let field name encoder value = `Assoc [(name, encoder value)]

  let (&) field_encoder row_encoder (field,row) =
    let field = field_encoder field in
    let row = row_encoder row in
    match field, row with
      | `Assoc field, `Assoc row -> `Assoc (List.rev_append field row)
      | `Assoc _, `Null -> field
      | _, _ -> assert false

  let record fields_encoder = fields_encoder

  let mapping key_encoder val_encoder m =
    let encoded_key k = match key_encoder k with
      | `String s -> s
      | json -> Json.to_string json
    in
    let encoded_pair (k,v) =
      (encoded_key k, val_encoder v)
    in
    let open Series.Bounded in
    `Assoc (Series.Mapping.pairs m |> map encoded_pair |> to_bag)
end

let format = Serialization.{
  decoder = (module Decoder);
  encoder = (module Encoder);
}
