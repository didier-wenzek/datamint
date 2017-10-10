(* see: http://okmij.org/ftp/ML/first-class-modules/generics.ml *)

module type S =
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
