type name = string

type resource =
  | Logger of Logger.logger
  | Publisher of Publisher.publisher

module Env : sig
  type 'a t

  val empty: 'a t
  val add_resource: name -> 'a -> 'a t -> 'a t
  val set_default: 'a option -> 'a t -> 'a t
  val find: 'a t -> name -> 'a option
end

val open_resources:
     (name * Logger.logger_kind) list
  -> (name * Publisher.publisher_kind) list
  -> resource Env.t Lwt.t
