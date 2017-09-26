type multiplicity =
  | PartialPlural       (* zero or more *)
  | PartialSingular     (* at most one *)
  | TotalSingular       (* exactly one *)
  | TotalPlural         (* at least one *)

val is_partial: multiplicity -> bool
val is_total: multiplicity -> bool
val is_singular: multiplicity -> bool
val is_plural: multiplicity -> bool

val multiplicity: partial:bool -> plural:bool -> multiplicity

val combine_multiplicity: multiplicity -> multiplicity -> multiplicity

val make_partial: multiplicity -> multiplicity
val make_total: multiplicity -> multiplicity
val make_singular: multiplicity -> multiplicity
val make_plural: multiplicity -> multiplicity
