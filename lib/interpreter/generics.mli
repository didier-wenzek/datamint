module type Interpretation =
sig
  type 'a tc

  (* Primitive types *)
  val unit: unit tc
  val int: int tc
  val int64  : int64 tc
  val float  : float tc
  val bool: bool tc
  val string: string tc

  (* Tuples *)
  val pair: 'a tc -> 'b tc -> ('a * 'b) tc
  val tuple: 'a tc -> 'a tc

  (* Records *)
  val empty_record: unit tc
  val field: string -> 'a tc -> 'a tc
  val (&): 'a tc -> 'row tc -> ('a * 'row) tc
  val record: 'row tc -> 'row tc

  (* Lists *)
  val list: 'a tc -> 'a Series.Bounded.producer tc

  (* Maps *)
  val mapping: 'a tc -> 'b tc -> ('a,'b) Series.Mapping.t tc
end

module type Repr = sig
  type a
  module Interpret (I: Interpretation):
  sig
    val result: a I.tc
  end
end

type 'a repr = (module Repr with type a = 'a)

val unit: unit repr
val int: int repr
val int64: int64 repr
val float: float repr
val bool: bool repr
val string: string repr

val pair: 'a repr -> 'b repr -> ('a * 'b) repr
val tuple: 'a repr -> 'a repr

val empty_record: unit repr 
val field: string -> 'a repr -> 'a repr
val (&): 'a repr -> 'b repr -> ('a * 'b) repr
val record: 'a repr -> 'a repr

val list: 'a repr -> 'a Series.Bounded.producer repr

val mapping: 'a repr -> 'b repr -> ('a,'b) Series.Mapping.t repr
