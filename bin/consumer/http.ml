open Cohttp
open Cohttp_lwt_unix
open Lwt

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

let server port loggers =
  let mode = `TCP (`Port 8000) in
  let callback = callback loggers in
  Server.create ~mode (Server.make ~callback ())
