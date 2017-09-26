module type S = sig
  type 'a t
  type key

  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val lookup: key -> 'a t -> 'a option
  val bindings: 'a t -> (key * 'a) list
  val keys: 'a t -> key list
  val vals: 'a t -> 'a list
end

module Make(Ord: Map.OrderedType): S with type key = Ord.t
= struct
  module M = Map.Make(Ord)

  type 'a t = 'a M.t
  type key = Ord.t

  let empty = M.empty
  let add = M.add

  let lookup x m =
    try Some (M.find x m)
    with Not_found -> None

  let bindings = M.bindings
  let keys assocs = List.map fst (bindings assocs)
  let vals assocs = List.map snd (bindings assocs)
end
