module type S = sig
  type 'a t
  type 'a elt

  val single: 'a elt -> 'a t
  val merge: 'a t -> 'a t -> 'a t

  val push: 'a elt -> 'a t -> 'a t
  val push_all: 'a t -> 'a t -> 'a t
end

module type M = sig
  include S

  val empty: 'a t
end

module type G = sig
  include M

  val opposite: 'a t -> 'a t

  val opposite_single: 'a elt -> 'a t
  val remove: 'a elt -> 'a t -> 'a t
  val remove_all: 'a t -> 'a t -> 'a t
end

module SeqMonoid : M
  with type 'a elt = 'a
= struct
  type 'a t = Empty | Single of 'a | Merge of 'a t * 'a t
  type 'a elt = 'a

  let empty = Empty
  let single x = Single x
  let merge xs ys = Merge (xs,ys)

  let push x xs = Merge (xs, Single x)
  let push_all xs ys = Merge (ys, xs)
end

module SetMonoid(Elt: Set.OrderedType) : M
  with type 'a elt = Elt.t
= struct
  module S = Set.Make(Elt)

  type 'a t = S.t
  type 'a elt = S.elt

  let empty = S.empty
  let single = S.singleton
  let merge = S.union

  let push = S.add
  let push_all = S.union
end

module SetGroup(Elt: Set.OrderedType) : G
  with type 'a elt = Elt.t
= struct
  module S = Set.Make(Elt)

  type 'a t = { included: S.t ; excluded: S.t }
  type 'a elt = S.elt

  let empty = { included = S.empty; excluded = S.empty }
  let single x = { included = S.singleton x; excluded = S.empty }
  let opposite_single x = { included = S.empty; excluded = S.singleton x }

  let opposite xs = { included = xs.excluded; excluded = xs.included }

  let merge xs ys = 
    let included = S.union (S.diff xs.included ys.excluded) (S.diff ys.included xs.excluded) in
    let excluded = S.union (S.diff xs.excluded ys.included) (S.diff ys.excluded xs.included) in
    { included; excluded }

  let push x xs =
    if S.mem x xs.excluded
    then { xs with excluded = S.remove x xs.excluded }
    else { xs with included = S.add x xs.included }

  let remove x xs =
    if S.mem x xs.included
    then { xs with included = S.remove x xs.included }
    else { xs with excluded = S.add x xs.excluded }

  let push_all = merge

  let remove_all xs = push_all (opposite xs)
end

module BagGroup(Elt: Set.OrderedType) : G
  with type 'a elt = Elt.t
= struct
  module M = Map.Make(Elt)
 
  type 'a t = int M.t
  type 'a elt = M.key

  let find k kvs =
    try M.find k kvs
    with Not_found -> 0

  let update f k kvs =
    M.add k (f (find k kvs)) kvs

  let combine f xs = M.merge (fun _k a b -> match a,b with
    | Some a, Some b -> Some (f a b)
    | None, s | s, None -> s
  ) xs
 
  let empty = M.empty

  let single x = M.singleton x 1

  let merge = combine (+)
 
  let opposite = M.map (fun a -> -a)

  let opposite_single x = M.singleton x (-1)

  let push = update succ

  let push_all = merge

  let remove = update pred

  let remove_all xs ys = combine (-) ys xs
end

module Mapping(K: Map.OrderedType)(V: M) : M
  with type 'a elt = K.t * 'a V.elt
= struct
  module G = Map.Make(K) 

  type 'a t = 'a V.t G.t
  type 'a elt = K.t * 'a V.elt

  let empty = G.empty
  let single (k,v) = G.singleton k (V.single v)
  let merge kvs = G.merge (fun _k a b -> match a,b with
    | Some a, Some b -> Some (V.merge a b)
    | None, s | s, None -> s
  ) kvs

  let find k kvs =
    try G.find k kvs
    with Not_found -> V.empty

  let push (k,v) kvs = 
    G.add k (V.push v (find k kvs)) kvs

  let push_all kvs = G.merge (fun _k a b -> match a,b with
    | Some a, Some b -> Some (V.push_all a b)
    | None, s | s, None -> s
  ) kvs
end

module MappingOpt(K: Map.OrderedType)(V: S) : M
  with type 'a elt = K.t * 'a V.elt
= struct
  module G = Map.Make(K) 

  type 'a t = 'a V.t G.t
  type 'a elt = K.t * 'a V.elt

  let empty = G.empty
  let single (k,v) = G.singleton k (V.single v)
  let merge kvs = G.merge (fun _k a b -> match a,b with
    | Some a, Some b -> Some (V.merge a b)
    | None, s | s, None -> s
  ) kvs

  let _find k kvs =
    try Some (G.find k kvs)
    with Not_found -> None

  let update k v kvs =
    try
      let v0 = G.find k kvs in
      V.push v v0
    with Not_found ->
      V.single v

  let push (k,v) kvs =
    G.add k (update k v kvs) kvs

  let push_all kvs = G.merge (fun _k a b -> match a,b with
    | Some a, Some b -> Some (V.push_all a b)
    | None, s | s, None -> s
  ) kvs
end

module MappingRev(K: Map.OrderedType)(V: G) : G
  with type 'a elt = K.t * 'a V.elt
= struct
  module G = Map.Make(K) 

  type 'a t = 'a V.t G.t
  type 'a elt = K.t * 'a V.elt

  let empty = G.empty
  let single (k,v) = G.singleton k (V.single v)
  let opposite_single (k,v) = G.singleton k (V.opposite_single v)
  let merge kvs = G.merge (fun _k a b -> match a,b with
    | Some a, Some b -> Some (V.merge a b)
    | None, s | s, None -> s
  ) kvs

  let opposite kvs = G.map V.opposite kvs

  let find k kvs =
    try G.find k kvs
    with Not_found -> V.empty

  let push (k,v) kvs = 
    G.add k (V.push v (find k kvs)) kvs

  let push_all kvs = G.merge (fun _k a b -> match a,b with
    | Some a, Some b -> Some (V.push_all a b)
    | None, s | s, None -> s
  ) kvs

  let remove (k,v) kvs =
    G.add k (V.remove v (find k kvs)) kvs

  let remove_all kvs = G.merge (fun _k a b -> match a,b with
    | Some a, Some b -> Some (V.remove_all a b)
    | None, s | s, None -> s
  ) kvs
end
