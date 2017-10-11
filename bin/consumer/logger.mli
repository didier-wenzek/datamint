type topic = string
type event = string
type logger = topic -> event -> unit Lwt.t

module Env : sig
  type t

  val empty: t
  val add_logger: topic -> logger -> t -> t
  val set_default_logger: logger option -> t -> t

  val find: t -> topic -> logger option
end

val stdout: logger
val stderr: logger
val file: string -> logger Lwt.t
