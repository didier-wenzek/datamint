(** A relation is represented by a multiplicity triple.

  - The multiplicity of pairs.
  - The multiplicity of right values given a left value.
  - The multiplicity of left values given a right value. *)
type ('a,'b,'ab_gen,'a_gen,'b_gen) relation = {
  gen: (Multiplicity.t, 'ab_gen) Capability.t;
  map: (Multiplicity.t, 'a_gen) Capability.t;
  inv: (Multiplicity.t, 'b_gen) Capability.t;
}

(** A collection is represented by the element multiplicity *)
type ('a,'a_gen) collection = (Multiplicity.t, 'a_gen) Capability.t

(** A reducer is represented by the element multiplicity of an aggregate *)
type ('a,'b) reducer = Multiplicity.t

(** A stream of records is summarized by the multiplicity of elements *)
type 'a records = Multiplicity.t

(** Records and field are unused *)
type ('a,'b) extractor = 'a -> 'b
type ('a,'b,'c) injector = 'a -> 'b -> 'c

let record_source = Multiplicity.unit

let project f m = m
let generate rel _ _ = Multiplicity.mult (Capability.get rel.gen)
let map      rel _ _ = Multiplicity.mult (Capability.get rel.map)
let inv_map  rel _ _ = Multiplicity.mult (Capability.get rel.inv)
let filter   rel _ _ = Multiplicity.make_partial

let gen_cap rel = Capability.map (fun factor _ _ -> Multiplicity.mult factor) rel.gen
let map_cap rel = Capability.map (fun factor _ _ -> Multiplicity.mult factor) rel.map
let inv_cap rel = Capability.map (fun factor _ _ -> Multiplicity.mult factor) rel.inv

let generate_member col _ = Multiplicity.mult (Capability.get col)
let filter_member   col _ = Multiplicity.make_partial

let reduce red _ _         = Capability.some red
let group _ _ m            = { gen = Capability.some m; map = Capability.some Multiplicity.at_least_one; inv = Capability.some Multiplicity.zero_or_more; }
let group_reduce red _ _ m = { gen = Capability.some m; map = Capability.some red; inv = Capability.some Multiplicity.zero_or_more; }

let rel_of_col col = { gen = col; map = col; inv = Capability.some Multiplicity.at_most_one; }
let col_of_rel rel = rel.gen

