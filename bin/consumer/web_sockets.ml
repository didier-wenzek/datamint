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

let log_errors ~task_name ~on_error task arg =
  Lwt.catch (fun () -> task arg) (fun exn ->
    Lwt_io.eprintf "ERROR failed to %s: %s\n%!" task_name (Printexc.to_string exn)
    >>= fun () -> on_error
  )

let log_frame logger client resource frame =
  logger resource frame.Websocket.Frame.content

let logger_loop client resource logger =
  let step () =
    Connected_client.recv client
    >>= fun frame -> 
    match frame.Frame.opcode with
    | Text | Binary -> (
      log_frame logger client resource frame
      >>= fun () ->
      Lwt.return_true
    )
    | Close -> Lwt.return_false
    | _ -> Lwt.return_true
  in
  let rec loop () =
    log_errors ~task_name:"consume a frame from a websocket" ~on_error:Lwt.return_true step ()
    >>= fun continue ->
    if continue
    then loop ()
    else Lwt.return_unit
  in
  loop ()

let publisher_loop client resource publisher =
  let push message =
    let frame = Websocket.Frame.create ~content:message () in
    Websocket_lwt.Connected_client.send client frame
  in
  publisher (log_errors ~task_name:"publish a frame on a websocket" ~on_error:Lwt.return_unit push)

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
  let callback =
    log_errors ~task_name:"establish a websocket connection" ~on_error:Lwt.return_unit
    (callback loggers)
  in
  let on_exn exn =
    Printf.eprintf "ERROR failed to establish a websocket connection: %s\n%!" (Printexc.to_string exn)
  in
  establish_standard_server ~mode ~check_request ~on_exn callback
