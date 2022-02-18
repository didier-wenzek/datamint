open Cmdliner
open Lwt.Infix

let info =
  let doc = "Generate random PassOver input events." in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname)";
  ] in
  Cmd.info "passover-event-generator" ~doc ~man

let kafka_host =
  let doc = "Connection string to the Kafka cluster (comma separated list of hostnames or hostname:port pairs) ." in
  Arg.(value & opt string "localhost" & info ["k"; "kafka"] ~docv:"KAFKA_CLUSTER" ~doc)

let positions_messages = "passover-positions-messages"

let new_event () =
  let visitor = 2 + (Random.int 998) in
  let x = Random.int 4 in
  let y = Random.int 5 in
  let t = Unix.gettimeofday () |> ( *.) 1000. |> int_of_float in
  Printf.sprintf "{ \"visitor\": \"V%.3d\", \"room\": \"R%d.%d\", \"timestamp\": %d }" visitor x y t

let gen_events push =
  let rec loop () =
    push (new_event ()) >>= loop 
  in
  loop ()

let connect_topic kafka_cluster topic_name =
  let kafka_options = ["metadata.broker.list",kafka_cluster] in
  let topic_options = [] in
  let handler = Kafka.new_producer kafka_options in
  let topic = Kafka.new_topic handler topic_name topic_options in
  (handler, topic)

let publish topic msg =
  Lwt.return ( try 
    Kafka.produce topic Kafka.partition_unassigned msg
    with exn -> (
      Printf.eprintf "ERROR : %s\n%!" (Printexc.to_string exn);
      Unix.sleep 1)
  )

let runner kafka_host =
  Lwt_main.run (
    let (_handler, topic) = connect_topic kafka_host positions_messages in
    gen_events (publish topic)
  )

let runner_t =
  Term.(const runner $ kafka_host)

let main () =
  Cmd.v info runner_t |> Cmd.eval |> exit

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  main ()
