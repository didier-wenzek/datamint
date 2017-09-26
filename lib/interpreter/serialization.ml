module Result = struct

  type 'a t = ('a,string) result

  let ok s = Ok s
  let error e = Error e

  let map f = function
    | Ok x -> Ok (f x)
    | Error _ as e -> e  (* If the case is coded as `| e -> e`,
                            Then the compiler wrongly infers the type of `map` as `('a -> 'a) -> 'a t -> 'a t` *)

  let try_map f x =
    try Ok (f x)
    with e -> Error (Printexc.to_string e)

  let flat_map f = function
    | Ok x -> f x
    | Error _ as e -> e

  let (>>=) r f = match r with
    | Ok x -> f x
    | Error _ as e -> e

  let result_list_map f =
    let rec loop rs = function
      | [] -> Ok (List.rev rs)
      | x::xs -> (
        match f x with
        | Ok r -> loop (r::rs) xs
        | Error r -> Error r
      )
    in
    loop []

  let to_collection = function
    | Ok x -> Series.Bounded.singleton x
    | Error _ -> Series.Bounded.empty

end

module type Decoder = sig
  type 'a decoder

  val decode: 'a decoder -> string -> 'a Result.t

  include Generics.Interpretation with type 'a tc = 'a decoder
end

module type Encoder = sig
  type 'a encoder

  val encode: 'a encoder -> 'a -> string Result.t

  include Generics.Interpretation with type 'a tc = 'a encoder
end

module type Encoding = sig
  type 'a t

  val decode: 'a t -> string -> 'a Result.t
  val encode: 'a t -> 'a -> string Result.t

  include Generics.Interpretation with type 'a tc = 'a t
end

type format = {
  decoder: (module Decoder);
  encoder: (module Encoder);
}

type 'a decoder = 'a Generics.repr -> string -> 'a Result.t
type 'a encoder = 'a Generics.repr -> 'a -> string Result.t

let format_decoder: format -> 'a Generics.repr -> string -> 'a Result.t =
  fun format -> 
    let format_decoder = format.decoder in
    let module F = (val format_decoder : Decoder) in
    let decoder = Generics.(
      fun (type a) repr -> 
        let module R = (val repr : Repr with type a = a) in 
        let module N = R.Interpret (F)
        in N.result
    ) in
    fun repr ->
      F.decode (decoder repr)

let format_encoder: format -> 'a Generics.repr -> 'a -> string Result.t =
  fun format ->
    let format_encoder = format.encoder in
    let module F = (val format_encoder: Encoder) in
    let encoder = Generics.(
      fun (type a) repr -> 
        let module R = (val repr : Repr with type a = a) in 
        let module N = R.Interpret (F)
        in N.result
    ) in
    fun repr ->
      F.encode (encoder repr)
