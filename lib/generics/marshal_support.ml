module Decoder = struct
  type 'a decoder =
    | Id : (string -> 'a) -> 'a decoder
    | Unpack : (string -> 'b) * ('b -> 'a) -> 'a decoder

  type 'a tc = 'a decoder

  let decode decoder s =
    try match decoder with
      | Id unmarshal -> Ok (unmarshal s)
      | Unpack (unmarshal, unpack) -> Ok (unpack (unmarshal s))
    with e -> Error (Printexc.to_string e)

  let unmarshal s = Marshal.from_string s 0

  let string : string decoder = Id unmarshal
  let int : int decoder = Id unmarshal
  let int64 : int64 decoder = Id unmarshal
  let float : float decoder = Id unmarshal
  let bool : bool decoder = Id unmarshal
  let unit : unit decoder = Id unmarshal

  let list : 'a decoder -> 'a Series.Bounded.producer decoder
    = function
    | Id _ ->
      Unpack (unmarshal, Series.Bounded.of_list)
    | Unpack (_, unpack_item) ->
      let unpack xs =
        Series.Bounded.map unpack_item (Series.Bounded.of_list xs)
      in
      Unpack (unmarshal, unpack)

  let pair head_decoder tail_decoder =
    match head_decoder, tail_decoder with
    | Id _, Id _ ->
      Id unmarshal
    | Id _, Unpack (_, unpack_tail) ->
      Unpack (unmarshal, (fun (h,t) -> (h, unpack_tail t)))
    | Unpack (_, unpack_head), Id _ ->
      Unpack (unmarshal, (fun (h,t) -> (unpack_head h, t)))
    | Unpack (_, unpack_head), Unpack (_, unpack_tail) ->
      Unpack (unmarshal, (fun (h,t) -> (unpack_head h, unpack_tail t)))

  let tuple pair_decoder = pair_decoder

  let empty_record = unit
  let field _name decoder = decoder
  let (&) = pair
  let record fields_decoder = fields_decoder

  let mapping key_decoder val_decoder =
    match list (pair key_decoder val_decoder) with
    | Id _ ->
      Unpack (unmarshal, Series.Mapping.of_pairs)
    | Unpack (_, unpack_pairs) ->
      Unpack (unmarshal, fun pairs -> Series.Mapping.of_pairs (unpack_pairs pairs))
     
end

module Encoder = struct
  type 'a encoder =
    | Id : ('a -> string) -> 'a encoder
    | Pack : ('a -> 'b) * ('b -> string) -> 'a encoder

  type 'a tc = 'a encoder

  let encode encoder x =
    try match encoder with
      | Id marshal -> Ok (marshal x)
      | Pack (pack,marshal) -> Ok (marshal (pack x))
    with e -> Error (Printexc.to_string e)

  let marshal x = Marshal.to_string x []

  let string : string encoder = Id marshal
  let int : int encoder = Id marshal
  let int64 : int64 encoder = Id marshal
  let float : float encoder = Id marshal
  let bool : bool encoder = Id marshal
  let unit : unit encoder = Id marshal

  let list = function
    | Id _ ->
      Pack (Series.Bounded.to_list, marshal)
    | Pack (pack_item,_) ->
      let pack xs =
        Series.Bounded.to_list (Series.Bounded.map pack_item xs)
      in
      Pack (pack, marshal)

  let pair head_encoder tail_encoder =
    match head_encoder, tail_encoder with
    | Id _, Id _ ->
      Id marshal
    | Id _, Pack (pack_tail,_) ->
      Pack ((fun (h,t) -> (h, pack_tail t)), marshal)
    | Pack (pack_head,_), Id _->
      Pack ((fun (h,t) -> (pack_head h, t)), marshal)
    | Pack (pack_head,_), Pack (pack_tail,_) ->
      Pack ((fun (h,t) -> (pack_head h, pack_tail t)), marshal)

  let tuple pair_encoder = pair_encoder

  let empty_record = unit
  let field _name encoder = encoder
  let (&) = pair
  let record fields_encoder = fields_encoder

  let mapping key_encoder val_encoder =
    match list (pair key_encoder val_encoder) with
    | Id _ ->
      Pack (Series.Mapping.pairs, marshal)
    | Pack (pack_pairs, _) ->
      Pack ((fun m -> pack_pairs (Series.Mapping.pairs m)), marshal)
end

let format = Serialization.{
  decoder = (module Decoder);
  encoder = (module Encoder);
}
