type t =
  | PartialPlural       (* zero or more *)
  | PartialSingular     (* at most one *)
  | TotalSingular       (* exactly one *)
  | TotalPlural         (* at least one *)

let is_singular = function
  | TotalSingular | PartialSingular -> true
  | _ -> false

let is_plural = function
  | PartialPlural | TotalPlural -> true
  | _ -> false

let is_partial = function
  | PartialPlural | PartialSingular -> true
  | _ -> false

let is_total = function
  | TotalSingular | TotalPlural -> true
  | _ -> false

let multiplicity ~partial ~plural =
  if partial
  then if plural then PartialPlural else PartialSingular
  else if plural then TotalPlural else TotalSingular

let unit = TotalSingular

let mult r s =
  let partial = is_partial r || is_partial s in
  let plural = is_plural r || is_plural s in
  multiplicity  ~partial ~plural

let make_singular = function
  | PartialPlural -> PartialSingular
  | TotalPlural -> TotalSingular
  | m -> m

let make_plural = function
  | PartialSingular -> PartialPlural
  | TotalSingular -> TotalPlural
  | m -> m

let make_partial = function
  | TotalSingular -> PartialSingular
  | TotalPlural -> PartialPlural
  | m -> m

let make_total = function
  | PartialPlural -> TotalPlural
  | PartialSingular -> TotalSingular
  | m -> m
