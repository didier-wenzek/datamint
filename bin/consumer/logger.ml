open Sexplib.Std

type topic = string [@@deriving sexp]
type event = string [@@deriving sexp]
type logger = topic -> event -> unit Lwt.t

type logger_kind =
  | Stdout
  | Stderr
  | File of string
  [@@deriving sexp]

type config = topic * logger_kind
  [@@deriving sexp]

let stdout = Lwt_io.printf "%s: %s\n%!"
let stderr = Lwt_io.eprintf "%s: %s\n%!"

open Lwt.Infix

let open_append_only path =
  let flags = Unix.[O_WRONLY; O_NONBLOCK; O_APPEND; O_CREAT] in
  let perm = 0o640 in
  let mode = Lwt_io.output in
  Lwt_io.open_file ~flags ~perm ~mode path

let file path =
  open_append_only path
  >|= fun cout ->
  Lwt_io.fprintf cout "%s: %s\n%!"

let logger_of_kind = function
  | Stdout -> Lwt.return stdout
  | Stderr -> Lwt.return stderr
  | File path -> file path

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

  let find env t =
    try
      Some (Dict.find t env.loggers)
    with
      Not_found -> env.default

  let add_config env (topic,kind) =
    logger_of_kind kind
    >|= fun logger ->
    add_logger topic logger env

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
