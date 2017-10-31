open Sexplib
open Sexplib.Std

type endpoint =
  | Http of Http.config
  | Ws of Web_sockets.config
  [@@deriving sexp]

type config =
  { endpoints: endpoint list;
    loggers: Logger.config list;
    publishers: Publisher.config list;
  } [@@deriving sexp]

let open_loggers config =
  Resource.open_resources Logger.make_logger config.loggers

let open_publishers config =
  Resource.open_resources Publisher.make_publisher config.publishers

let endpoint_server = function
  | Http config -> Http.server config
  | Ws config -> Web_sockets.server config

let endpoints config =
  config.endpoints
  |> List.map endpoint_server

let load path =
  Sexp.load_sexps path
  |> fun s -> Sexp.List s
  |> config_of_sexp
