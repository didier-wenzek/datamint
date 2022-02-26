module Cap = struct
  type t =  [ `Yes | `No ]
  type yes =  [ `Yes ]
  type no =  [ `No ]

  type ('a,'b) impl =
    | Cap : 'a -> ('a, yes) impl
    | NoCap : ('a, no) impl 

  let get = function
    | Cap a -> a

  let map (type a) (type b) (type c): (a -> b) -> (a,c) impl -> (b,c) impl
    = fun f -> function
    | Cap a -> Cap (f a)
    | NoCap -> NoCap
end

type ('a,'b) extractor = 'a -> 'b
type ('a,'b,'c) injector = 'a -> 'b -> 'c

module type S = sig
  type ('a,'b,'gen_cap,'map_cap,'inv_cap,'chk_cap) relation
  type 'a value

  module Gen = Cap
  module Map = Cap
  module Inv = Cap
  module Chk = Cap

  val generate: ('a,'b, Gen.yes,Map.t,Inv.t,Chk.t) relation -> ('c,'a,'d) injector ->  ('d,'b,'e) injector -> 'c value -> 'e value
  val map: ('a,'b, Gen.t,Map.yes,Inv.t,Chk.t) relation -> ('c,'a) extractor -> ('c,'b,'d) injector -> 'c value -> 'd value
  val inv_map: ('a,'b, Gen.t,Map.t,Inv.yes,Chk.t) relation -> ('c,'a,'d) injector -> ('c,'b) extractor -> 'c value -> 'd value
  val filter: ('a,'b, Gen.t,Map.t,Inv.t,Chk.yes) relation -> ('c,'a) extractor -> ('c,'b) extractor -> 'c value -> 'c value

  val rel_of_fun: ('a -> 'b) -> ('a,'b,Gen.no,Map.yes,Inv.no,Chk.yes) relation
  val inverse: ('a,'b,'c,'d,'e,'f) relation -> ('b,'a,'c,'e,'d,'f) relation
end

module Impl(Dataset: Dataset.S) : S = struct

  module Gen = Cap
  module Map = Cap
  module Inv = Cap
  module Chk = Cap

  type 'a dataset = 'a Dataset.t
  type 'a value = 'a dataset

  type ('a,'b, 'gen_cap, 'map_cap, 'inv_cap, 'chk_cap) relation = {
    gen: (('a*'b)  dataset, 'gen_cap) Cap.impl;
    map: ('a -> 'b dataset, 'map_cap) Cap.impl;
    inv: ('b -> 'a dataset, 'inv_cap) Cap.impl;
    chk: ('a -> 'b -> bool, 'chk_cap) Cap.impl;
  }

  let filter rel =
    let chk = Cap.get rel.chk in
    fun get_a get_b -> Dataset.filter (fun c -> chk (get_a c) (get_b c))

  let map rel =
    let map = Cap.get rel.map in
    fun get_a set_b -> Dataset.flatmap (fun c -> map (get_a c) |> Dataset.map (set_b c))

  let inv_map rel =
    let inv = Cap.get rel.inv in
    fun set_a get_b -> Dataset.flatmap (fun c -> inv (get_b c) |> Dataset.map (set_a c))

  let generate rel =
    let gen = Cap.get rel.gen in
    fun set_a set_b ->
      let pair c (a,b) =
        let d = set_a c a 
        in set_b d b
      in
      Dataset.flatmap (fun c -> Dataset.map (pair c) gen)

  let rel = {
    gen = Cap.NoCap;
    map = Cap.NoCap;
    inv = Cap.NoCap;
    chk = Cap.NoCap;
  }

  let rel_of_fun f = {
    rel with
    map = Cap.Cap (fun x -> Dataset.singleton (f x));
    chk = Cap.Cap (fun x y -> f x = y);
  }

  let inverse rel = {
    gen = Cap.map (Dataset.map Util.swap_pair) rel.gen;
    map = rel.inv;
    inv = rel.map;
    chk = Cap.map Util.swap rel.chk;
  }
end

module type Plan = sig
  include S

  type ('a,'b) var
  type ('a,'b) query
  
  val _filter:   ('a,'c) var -> ('a,'b, Cap.t,Cap.t,Cap.t,Cap.yes) relation -> ('b,'c) var -> 'c value -> 'c value
  val _map:      ('a,'c) var -> ('a,'b, Cap.t,Cap.yes,Cap.t,Cap.t) relation -> ('c,'a) extractor -> ('c,'b,'d) injector -> 'c value -> 'd value
  val _inv_map:  ('a,'c) var -> ('a,'b, Cap.t,Cap.t,Cap.yes,Cap.t) relation -> ('c,'a,'d) injector -> ('c,'b) extractor -> 'c value -> 'd value
  val _generate: ('a,'c) var -> ('a,'b, Cap.yes,Cap.t,Cap.t,Cap.t) relation -> ('c,'a,'d) injector ->  ('d,'b,'e) injector -> 'c value -> 'e value
end

module type S_bis = sig
  module Gen = Cap
  module Map = Cap
  module Inv = Cap
  module Chk = Cap

  type ('gen_cap,'map_cap,'inv_cap,'chk_cap) rel_cap
  type gen_cap = (Gen.yes,Map.t,Inv.t,Chk.t) rel_cap
  type map_cap = (Gen.t,Map.yes,Inv.t,Chk.t) rel_cap
  type inv_cap = (Gen.t,Map.t,Inv.yes,Chk.t) rel_cap
  type chk_cap = (Gen.t,Map.t,Inv.t,Chk.yes) rel_cap
  
  type ('a,'b,'rel_cap) relation
    constraint 'rel_cap = ('gen_cap,'map_cap,'inv_cap,'chk_cap) rel_cap

  type 'a value

  val generate: ('a,'b, gen_cap) relation -> ('c,'a,'d) injector ->  ('d,'b,'e) injector -> 'c value -> 'e value
  val map: ('a,'b, map_cap) relation -> ('c,'a) extractor -> ('c,'b,'d) injector -> 'c value -> 'd value
  val inv_map: ('a,'b, inv_cap) relation -> ('c,'a,'d) injector -> ('c,'b) extractor -> 'c value -> 'd value
  val filter: ('a,'b, chk_cap) relation -> ('c,'a) extractor -> ('c,'b) extractor -> 'c value -> 'c value

  val rel_of_fun: ('a -> 'b) -> ('a,'b, (Gen.no,Map.yes,Inv.no,Chk.yes) rel_cap) relation
  val inverse: ('a,'b, ('c,'d,'e,'f) rel_cap) relation -> ('b,'a, ('c,'e,'d,'f) rel_cap) relation
end

module Impl_bis(Dataset: Dataset.S) : S_bis = struct

  module Gen = Cap
  module Map = Cap
  module Inv = Cap
  module Chk = Cap

  type 'a dataset = 'a Dataset.t
  type 'a value = 'a dataset

  type ('gen_cap,'map_cap,'inv_cap,'chk_cap) rel_cap = |
  type gen_cap = (Gen.yes,Map.t,Inv.t,Chk.t) rel_cap
  type map_cap = (Gen.t,Map.yes,Inv.t,Chk.t) rel_cap
  type inv_cap = (Gen.t,Map.t,Inv.yes,Chk.t) rel_cap
  type chk_cap = (Gen.t,Map.t,Inv.t,Chk.yes) rel_cap

  type ('a,'b, 'rel_cap) relation = {
    gen: (('a*'b)  dataset, 'gen_cap) Cap.impl;
    map: ('a -> 'b dataset, 'map_cap) Cap.impl;
    inv: ('b -> 'a dataset, 'inv_cap) Cap.impl;
    chk: ('a -> 'b -> bool, 'chk_cap) Cap.impl;
  }
  constraint 'rel_cap = ('gen_cap,'map_cap,'inv_cap,'chk_cap) rel_cap

  let filter rel =
    let chk = Cap.get rel.chk in
    fun get_a get_b -> Dataset.filter (fun c -> chk (get_a c) (get_b c))

  let map rel =
    let map = Cap.get rel.map in
    fun get_a set_b -> Dataset.flatmap (fun c -> map (get_a c) |> Dataset.map (set_b c))

  let inv_map rel =
    let inv = Cap.get rel.inv in
    fun set_a get_b -> Dataset.flatmap (fun c -> inv (get_b c) |> Dataset.map (set_a c))

  let generate rel =
    let gen = Cap.get rel.gen in
    fun set_a set_b ->
      let pair c (a,b) =
        let d = set_a c a 
        in set_b d b
      in
      Dataset.flatmap (fun c -> Dataset.map (pair c) gen)

  let rel = {
    gen = Cap.NoCap;
    map = Cap.NoCap;
    inv = Cap.NoCap;
    chk = Cap.NoCap;
  }

  let rel_of_fun f = {
    rel with
    map = Cap.Cap (fun x -> Dataset.singleton (f x));
    chk = Cap.Cap (fun x y -> f x = y);
  }

  let inverse rel = {
    gen = Cap.map (Dataset.map Util.swap_pair) rel.gen;
    map = rel.inv;
    inv = rel.map;
    chk = Cap.map Util.swap rel.chk;
  }
end
