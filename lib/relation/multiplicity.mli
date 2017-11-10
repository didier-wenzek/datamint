type t =
  | PartialPlural       (** zero or more *)
  | PartialSingular     (** at most one *)
  | TotalSingular       (** exactly one *)
  | TotalPlural         (** at least one *)

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
