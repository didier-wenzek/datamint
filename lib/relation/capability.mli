(** Type annotation aimed to check at compilation time that some optional capability is implemented. *)

(** Phantom type aimed to tag that some capability is supported *)
type some

(** Phantom type aimed to tag that no capability is available *)
type none

(** A value of type [('a,some) t] wraps a value implementing a capability. *)
type ('a,'b) t

(** [some a] wraps the implementation [a] into a capability. *)
val some: 'a -> ('a, some) t

(** [none] is the capability without any implementation. *)
val none: ('a, none) t

(** [get cap] extracts the capability implementation. *)
val get: ('a, some) t -> 'a

(** [map f cap] updates the capability when supported. *)
val map: ('a -> 'b) -> ('a,'c) t -> ('b,'c) t
