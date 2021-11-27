(** A value of type [('a,'b,'b M.t) t], wraps an action with its monadic context. *)
type ('a,'b,'c) t = {
  push: 'a -> 'b -> 'c;
  cont: 'c -> ('b -> 'c) -> 'c;
  term: 'b -> 'c;
}

(** Wraps an action of type ['a -> 'b -> 'b], which simply returns the transformed state. *)
val sync_action: ('a -> 'b -> 'b) -> ('a, 'b, 'b) t

(** Wraps an action of type ['a -> 'b -> 'b Lwt], which returns a promise for the transformed state. *)
val lwt_action: ('a -> 'b -> 'b Lwt.t) -> ('a, 'b, 'b Lwt.t) t

(** Transform an action for early termination when the state reached a maximum. *)
val cap_with: ('b -> bool) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t

val map: ('a -> 'b) -> ('b, 'c, 'd) t -> ('a, 'c, 'd) t
val filter: ('a -> bool) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
val flat_map: iter:(('b,'c,'d) t -> 'bs -> 'c -> 'd) -> ('a -> 'bs) -> ('b, 'c, 'd) t -> ('a, 'c, 'd) t
