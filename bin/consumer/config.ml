open Sexplib
open Sexplib.Std

type config =
  { http: Http.config;
    loggers: Logger.config list;
  } [@@deriving sexp]

let loggers config =
  Logger.Env.of_configs config.loggers

let http_config config =
  Some config.http

let load path =
  Sexp.load_sexp path
  |> config_of_sexp
