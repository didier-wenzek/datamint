open Cmdliner
open Lwt
open Cohttp
open Cohttp_lwt_unix

let info =
  let doc = "Log the events received over misc network endpoints" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) URI LOGFILE
        record all  DATASET.";
  ] in
  Term.info "http_logger" ~doc ~man

let uri =
  let doc = "Filter events with the given uri" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"URI" ~doc)

let logfile =
  let doc = "File where the events are recorded" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"LOGFILE" ~doc)

let port =
  let doc = "Listen http requests on port $(docv)" in
  Arg.(value & opt int 8000 & info ["p"; "port"] ~docv:"PORT" ~doc)

let log req body =
  let uri = req |> Request.uri |> Uri.path in
  Printf.printf "%s: %s\n%!" uri body;
  Lwt.return (Printf.sprintf "New %s\n" uri)

let callback _conn req body =
  try_bind
    (fun () -> Cohttp_lwt.Body.to_string body >>= log req)
    (fun res -> Server.respond_string ~status:`Created ~body:res ())
    (fun err -> Server.respond_string ~status:`Bad_request ~body:"ERROR" ())

let server port uri logfile =
  let mode = `TCP (`Port port) in
  Server.create ~mode (Server.make ~callback ())
  |> Lwt_main.run

let server_t =
  Term.(pure server $ port $ uri $ logfile)

let main () =
  match Term.eval (server_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_unix.with_async_detach main
