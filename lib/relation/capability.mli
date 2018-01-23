(** Type annotation aimed to check at compilation time that some optional capability is implemented. *)

(** A value of type [('a,'b) t] may or may not provide a value of type ['a], depending of the type ['b].

    A value of type [('a,some) t] provides the capability.
    A value of type [('a,none) y] provides no implementation. *)
type ('a,'b) t

(** Phantom type aimed to tag that some capability is supported *)
type some

(** Phantom type aimed to tag that no capability is available *)
type none

(** [some a] wraps the implementation [a] into a capability. *)
val some: 'a -> ('a, some) t

(** [none] is the capability without any implementation. *)
val none: ('a, none) t

(** [get cap] extracts the capability implementation. *)
val get: ('a, some) t -> 'a

(** [get_opt cap] extracts the capability implementation, if any *)
val get_opt: ('a, 'optional) t -> 'a option

(** [map f cap] promotes the capability to [f cap] when supported. *)
val map: ('a -> 'b) -> ('a,'c) t -> ('b,'c) t
