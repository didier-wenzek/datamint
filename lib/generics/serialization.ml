module Result = struct
  include Series.Util.Result

  type 'a t = ('a,string) result

  let try_apply f =
    try_apply f Printexc.to_string 

  let to_collection = function
    | Ok x -> Series.Bounded.singleton x
    | Error _ -> Series.Bounded.empty
end

module type Decoder = sig
  type 'a decoder

  val decode: 'a decoder -> string -> 'a Result.t

  include Interpretation.S with type 'a tc = 'a decoder
end

module type Encoder = sig
  type 'a encoder

  val encode: 'a encoder -> 'a -> string Result.t

  include Interpretation.S with type 'a tc = 'a encoder
end

module type Encoding = sig
  type 'a t

  val decode: 'a t -> string -> 'a Result.t
  val encode: 'a t -> 'a -> string Result.t

  include Interpretation.S with type 'a tc = 'a t
end

type format = {
  decoder: (module Decoder);
  encoder: (module Encoder);
}

type 'a decoder = 'a Repr.t -> string -> 'a Result.t
type 'a encoder = 'a Repr.t -> 'a -> string Result.t

let format_decoder: format -> 'a Repr.t -> string -> 'a Result.t =
  fun format -> 
    let format_decoder = format.decoder in
    let module F = (val format_decoder : Decoder) in
    let decoder = (
      fun (type a) repr -> 
        let module R = (val repr : Repr.S with type a = a) in 
        let module N = R.Interpret (F)
        in N.result
    ) in
    fun repr ->
      F.decode (decoder repr)

let format_encoder: format -> 'a Repr.t -> 'a -> string Result.t =
  fun format ->
    let format_encoder = format.encoder in
    let module F = (val format_encoder: Encoder) in
    let encoder = (
      fun (type a) repr -> 
        let module R = (val repr : Repr.S with type a = a) in 
        let module N = R.Interpret (F)
        in N.result
    ) in
    fun repr ->
      F.encode (encoder repr)
