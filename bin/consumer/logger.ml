open Sexplib.Std

type topic = string [@@deriving sexp]
type event = string [@@deriving sexp]
type logger = topic -> event -> unit Lwt.t

type logger_kind =
  | Stdout
  | Stderr
  | File of string
  | Kafka of string
  [@@deriving sexp]

type config = topic * logger_kind
  [@@deriving sexp]

let stdout = Lwt_io.printf "%s: %s\n%!"
let stderr = Lwt_io.eprintf "%s: %s\n%!"
let file = File_logger.file

open Lwt.Infix

let logger_of_kind = function
  | Stdout -> Lwt.return stdout
  | Stderr -> Lwt.return stderr
  | File path -> file path
  | Kafka topic -> Lwt.return (Kafka_logger.topic_logger topic)

module Dict = Map.Make(String)

module Env = struct
  type t = {
    loggers: logger Dict.t;
    default: logger option;
  }

  let empty = {
    loggers = Dict.empty;
    default = None;
  }

  let add_logger topic logger env = {
    env with
    loggers = Dict.add topic logger env.loggers
  }

  let set_default_logger default env = {
    env with default;
  }

  let add_logger_or_default topic logger env =
    if topic = "*"
    then set_default_logger (Some logger) env
    else add_logger topic logger env

  let find env t =
    try
      Some (Dict.find t env.loggers)
    with
      Not_found -> env.default

  let add_config env (topic,kind) =
    logger_of_kind kind
    >|= fun logger ->
    add_logger_or_default topic logger env

  let rec add_configs env = function
    | [] -> Lwt.return env
    | conf::confs -> (
      add_config env conf
      >>= fun env ->
      add_configs env confs
    )

  let open_configs =
    add_configs empty
end
