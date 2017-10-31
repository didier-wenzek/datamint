type 'a t
type name = string

type resource =
  | Logger of Logger.logger
  | Publisher of Publisher.publisher

val empty: 'a t
val add_resource: name -> 'a -> 'a t -> 'a t
val set_default: 'a option -> 'a t -> 'a t

val find: 'a t -> name -> 'a option

val open_resources:
     (name * Logger.logger_kind) list
  -> (name * Publisher.publisher_kind) list
  -> resource t Lwt.t
