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

let log_frame logger client topic frame =
  logger topic frame.Websocket.Frame.content

let client_loop client topic logger =
  let rec loop () =
    Connected_client.recv client
    >>= fun frame -> 
    match frame.Frame.opcode with
    | Text | Binary -> (
      log_frame logger client topic frame
      >>=
      loop
    )
    | Close -> Lwt.return_unit
    | _ -> loop ()
  in
  loop

let callback loggers client =
  let req = Connected_client.http_request client in
  let topic = Uri.path (Request.uri req) in
  match Logger.Env.find loggers topic with
  | None -> Lwt.fail Not_found
  | Some logger -> 
    client_loop client topic logger ()

let server config loggers =
  let mode = `TCP (`Port config.port) in
  let check_request _ = true in
  let callback = callback loggers in
  establish_standard_server ~mode ~check_request callback