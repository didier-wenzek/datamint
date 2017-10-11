open Cmdliner
open Lwt
open Cohttp
open Cohttp_lwt_unix

let info =
  let doc = "Event listener and logger." in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) CONF_FILE

        consumes and logs the events received over the network.";
  ] in
  Term.info "event_logger" ~doc ~man

let conf_file =
  let doc = "Configuration file" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"CONF_FILE" ~doc)

module HTTP = struct

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
end

let server conf_file =
  let loggers =
    Logger.Env.empty
    |> Logger.Env.set_default_logger (Some Logger.stderr)
  in
  HTTP.server 8000 loggers
  |> Lwt_main.run

let server_t =
  Term.(pure server $ conf_file)

let main () =
  match Term.eval (server_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_unix.with_async_detach main
