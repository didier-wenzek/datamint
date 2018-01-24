module type S = sig                                                                                                                                                                                   
  type 'a t
  type 'a elt
  type 'a res

  val empty: 'a t
  val insert: 'a elt -> 'a t -> 'a t
  val combine: 'a t -> 'a t -> 'a t
  val elements: 'a t -> 'a res Series.t
  val maximum_opt: ('a t -> bool) option
end

module Extend(M: S) = struct
  include M

  let single x = insert x empty

(*
  let insert_action =
    let action = Action.sync_action insert in
    match maximum_opt with
      | None -> action
      | Some maximum -> Action.cap_with maximum action

  let fold xs =
    Series.fold xs insert_action empty
*)

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
  let maximum_opt = None
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
  let maximum_opt = None
end

module Forall : S
  with type 'a t = bool
  and  type 'a elt = bool
  and  type 'a res = bool
= struct
  type 'a t = bool
  type 'a elt = bool
  type 'a res = bool

  let empty = true
  let insert = (&&)
  let combine = (&&)
  let elements = Series.single
  let maximum_opt = Some not
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
  let maximum_opt = None
end

