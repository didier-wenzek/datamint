open Sexplib
open Sexplib.Std

type config =
  { http: Http.config;
    loggers: Logger.config list;
  } [@@deriving sexp]

let open_loggers config =
  Logger.Env.open_configs config.loggers

let http_config config =
  config.http

let load path =
  Sexp.load_sexps path
  |> fun s -> Sexp.List s
  |> config_of_sexp
