open Cohttp
open Cohttp_lwt_unix
open Lwt
open Sexplib
open Sexplib.Std

type config =
  { port: int;
  } [@@deriving sexp]

let config_of_sexp s =
  try { port = int_of_sexp s }
  with e -> config_of_sexp s 

let log loggers req body =
  let topic = Uri.path (Request.uri req) in
  match Logger.Env.find loggers topic with
  | None -> Lwt.fail Not_found
  | Some callback -> (
    Cohttp_lwt.Body.to_string body
    >>= callback topic
  )
    
let callback loggers _conn req body =
  try_bind
    (fun () -> log loggers req body)
    (fun res -> Server.respond_string ~status:`Created ~body:"Created" ())
    (fun err -> Server.respond_string ~status:`Bad_request ~body:"ERROR" ())

let server config loggers =
  let mode = `TCP (`Port config.port) in
  let callback = callback loggers in
  Server.create ~mode (Server.make ~callback ())
