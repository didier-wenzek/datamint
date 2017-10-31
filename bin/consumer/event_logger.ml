open Cmdliner
open Lwt.Infix

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

let launch_threads threads resources =
  threads
  |> List.map (fun thread -> thread resources)
  |> Lwt.join

let launch_server config =
  Config.open_resources config
  >>=
  launch_threads (Config.endpoints config)

let server conf_file =
  Config.load conf_file
  |> launch_server
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
