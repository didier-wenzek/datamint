type resource = string
type event = string
type logger = resource -> event -> unit Lwt.t

type logger_kind =
  | Stdout
  | Stderr
  | File of string
  | Kafka of string

type config = resource * logger_kind
  [@@deriving sexp]

module Env : sig
  type t

  val empty: t
  val add_logger: resource -> logger -> t -> t
  val set_default_logger: logger option -> t -> t

  val find: t -> resource -> logger option

  val open_configs: config list -> t Lwt.t
end

val stdout: logger
val stderr: logger
val file: string -> logger Lwt.t
