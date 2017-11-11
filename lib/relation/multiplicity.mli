type t =
  | PartialPlural       (** zero or more *)
  | PartialSingular     (** at most one *)
  | TotalSingular       (** exactly one *)
  | TotalPlural         (** at least one *)

val zero_or_more: t
val at_most_one: t
val exactly_one: t
val at_least_one: t

val is_partial: t -> bool
val is_total: t -> bool
val is_singular: t -> bool
val is_plural: t -> bool

val multiplicity: partial:bool -> plural:bool -> t

val unit: t
val mult: t -> t -> t

val make_partial: t -> t
val make_total: t -> t
val make_singular: t -> t
val make_plural: t -> t
