open Lwt
open Cohttp
open Websocket
open Websocket_lwt
open Sexplib.Std

type config =
  { port: int;
  } [@@deriving sexp]

let config_of_sexp s =
  try { port = int_of_sexp s }
  with e -> config_of_sexp s 

let log_frame logger client resource frame =
  logger resource frame.Websocket.Frame.content

let logger_loop client resource logger =
  let rec loop () =
    Connected_client.recv client
    >>= fun frame -> 
    match frame.Frame.opcode with
    | Text | Binary -> (
      log_frame logger client resource frame
      >>=
      loop
    )
    | Close -> Lwt.return_unit
    | _ -> loop ()
  in
  loop ()

let publisher_loop client resource publisher =
  let push message =
    let frame = Websocket.Frame.create ~content:message () in
    Websocket_lwt.Connected_client.send client frame
  in
  publisher push

let callback loggers client =
  let req = Connected_client.http_request client in
  let resource = Uri.path (Request.uri req) in
  match Resource.Env.find loggers resource with
  | None -> Lwt.fail Not_found
  | Some (Resource.Logger logger) -> 
    logger_loop client resource logger
  | Some (Resource.Publisher publisher) -> 
    publisher_loop client resource publisher

let server config loggers =
  let mode = `TCP (`Port config.port) in
  let check_request _ = true in
  let callback = callback loggers in
  establish_standard_server ~mode ~check_request callback
