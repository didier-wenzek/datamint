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
