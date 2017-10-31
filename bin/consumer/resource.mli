type 'a t
type name = string

val empty: 'a t
val add_resource: name -> 'a -> 'a t -> 'a t
val set_default: 'a option -> 'a t -> 'a t

val find: 'a t -> name -> 'a option

val open_resources: ('a -> 'b Lwt.t) -> (name * 'a) list -> 'b t Lwt.t
