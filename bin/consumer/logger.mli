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

val stdout: logger
val stderr: logger
val file: string -> logger Lwt.t

val make_logger: logger_kind -> logger Lwt.t
