type t
type index = VZ | VS of t

val index: t -> index
val zero: t
val succ: t -> t
val of_int: int -> t

val compare: t -> t -> int

(* Return the de_bruijn idx of a value in a list of values.
   Raise Not_found if the value is not found. *)
val idx_of: 'a -> 'a list -> t

(* Find the element of a list having a given de_bruijn idx.
   Raise Not_found if the matching idx is not found. *)
val find: t -> 'a list -> 'a

val show: t -> string

(* Return a type variable name *)
val name: t -> string

(* Assign a type variable name to each variables of a list.

   [names xs] is equivalent to [map (x -> name (find x xs)) xs]
 *)
val names: 'a list -> string list

(* Return the de_bruijn idx of the value in the list of values. *)
val find_eq: ('a -> 'a -> bool ) -> 'a list -> 'a -> t
