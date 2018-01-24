module type S = sig                                                                                                                                                                                   
  type 'a t
  type 'a elt
  type 'a res

  val empty: 'a t
  val insert: 'a elt -> 'a t -> 'a t
  val combine: 'a t -> 'a t -> 'a t
  val elements: 'a t -> 'a res Series.t
end

module Extend(M: S) = struct
  include M

  let single x = insert x empty

  let fold red xs = Series.fold_sync xs insert empty
end

module Sum : S
  with type 'a t = int
  and  type 'a elt = int
  and  type 'a res = int
= struct
  type 'a t = int
  type 'a elt = int
  type 'a res = int

  let empty = 0
  let insert = (+)
  let combine = (+)
  let elements = Series.single
end

module Count : S
  with type 'a t = int
  and  type 'a elt = 'a
  and  type 'a res = int
= struct
  type 'a t = int
  type 'a elt = 'a
  type 'a res = int
                                                                                                                                                                                                           
  let empty = 0
  let insert x = succ
  let combine = (+)
  let elements = Series.single
end

module Bag : S                                                                                                                                                                                        
  with type 'a t = 'a list
  and  type 'a elt = 'a
  and  type 'a res = 'a
= struct
  type 'a t = 'a list
  type 'a elt = 'a
  type 'a res = 'a

  let empty = []
  let insert = List.cons
  let combine = List.rev_append
  let elements = Series.of_list
end

