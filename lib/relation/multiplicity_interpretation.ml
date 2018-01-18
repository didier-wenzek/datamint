(** A relation is represented by a multiplicity triple.

  - The multiplicity of pairs.
  - The multiplicity of right values given a left value.
  - The multiplicity of left values given a right value. *)
type ('a,'b,'ab_gen,'a_gen,'b_gen) relation = {
  gen: Multiplicity.t;
  map: Multiplicity.t;
  inv: Multiplicity.t;
}

(** A collection is represented by the element multiplicity *)
type ('a,'a_gen) collection = Multiplicity.t

(** A reducer is represented by the element multiplicity of an aggregate *)
type ('a,'b) reducer = Multiplicity.t

(** A stream of records is summarized by the multiplicity of elements *)
type 'a records = Multiplicity.t

(** Records and field are unused *)
type ('a,'b) extractor = unit
type ('a,'b,'c) injector = unit

let record_source = Multiplicity.unit

let generate rel _ _ = Multiplicity.mult rel.gen
let map      rel _ _ = Multiplicity.mult rel.map
let inv_map  rel _ _ = Multiplicity.mult rel.inv
let filter   rel _ _ = Multiplicity.make_partial

let generate_member col _ = Multiplicity.mult col
let filter_member   col _ = Multiplicity.make_partial

let reduce red _ _         = red
let group _ _ m            = { gen = m; map = Multiplicity.at_least_one; inv = Multiplicity.zero_or_more }
let group_reduce red _ _ m = { gen = m; map = red; inv = Multiplicity.zero_or_more } 

let rel_of_col col = { gen = col; map = col; inv = Multiplicity.at_most_one }
let col_of_rel rel = rel.gen
