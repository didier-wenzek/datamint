module type S = sig
  type 'a t
  type ('a,'b) mapping

  val empty: 'a t
  val singleton: 'a -> 'a t
  val optional: 'a option -> 'a t
  val merge: 'a t -> 'a t -> 'a t
  val add: 'a -> 'a t -> 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val filter: ('a -> bool) -> 'a t -> 'a t
  val flatmap: ('a -> 'b t) -> 'a t -> 'b t
  val unnest: ('a -> 'b t) -> ('a -> 'b -> 'c) -> 'a t -> 'c t
  val exists: ('a -> bool) -> 'a t -> bool
  val flatten: 'a t t -> 'a t

  val fold: 'b -> ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a t -> 'b
  val aggregate: (unit -> 'b) -> ('a -> 'b -> 'b) -> ('b -> 'b -> 'b) -> 'a t -> 'b

  val of_list: 'a list -> 'a t

  val keys: ('a,'b) mapping -> 'a t
  val values: ('a,'b) mapping -> 'a -> 'b t
end
