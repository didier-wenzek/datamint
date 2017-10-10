module type S = sig
  type a
  module Interpret (I: Interpretation.S):
  sig
    val result: a I.tc
  end
end

type 'a t = (module S with type a = 'a)

val unit: unit t
val int: int t
val int64: int64 t
val float: float t
val bool: bool t
val string: string t

val pair: 'a t -> 'b t -> ('a * 'b) t
val tuple: 'a t -> 'a t

val empty_record: unit t 
val field: string -> 'a t -> 'a t
val (&): 'a t -> 'b t -> ('a * 'b) t
val record: 'a t -> 'a t

val list: 'a t -> 'a Series.Bounded.producer t

val mapping: 'a t -> 'b t -> ('a,'b) Series.Mapping.t t
