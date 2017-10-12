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

let server_thread config =
  Config.open_loggers config
  >>=
  Http.server (Config.http_config config)

let server conf_file =
  Config.load conf_file
  |> server_thread
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
