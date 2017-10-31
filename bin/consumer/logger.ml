open Sexplib.Std

type resource = string [@@deriving sexp]
type event = string [@@deriving sexp]
type logger = resource -> event -> unit Lwt.t

type logger_kind =
  | Stdout
  | Stderr
  | File of string
  | Kafka of string
  [@@deriving sexp]

type config = resource * logger_kind
  [@@deriving sexp]

let stdout = Lwt_io.printf "%s: %s\n%!"
let stderr = Lwt_io.eprintf "%s: %s\n%!"
let file = File_logger.file

open Lwt.Infix

let make_logger = function
  | Stdout -> Lwt.return stdout
  | Stderr -> Lwt.return stderr
  | File path -> file path
  | Kafka topic -> Lwt.return (Kafka_logger.topic_logger topic)

