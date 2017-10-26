open Cmdliner
open Message
open Series
open Util
module Dataset = Incremental_value

let info =
  let doc = "Run PassOver continuous queries." in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) PARTITION PARTITION_COUNT

        Continuously consumes visitor events,
        aggregates visitor data
        and produces visitor alerts.";
  ] in
  Term.info "passover" ~doc ~man

let partition =
  let doc = "partition this instance will consume (for all Kafka topics)" in
  Arg.(required & pos 0 (some int) None & info [] ~docv:"PARTITION" ~doc)

let partition_count =
  let doc = "count of partitions (for all Kafka topics)" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"PARTITION_COUNT" ~doc)

let kafka_host =
  let doc = "Connection string to the Kafka cluster." in
  Arg.(value & opt string "localhost" & info ["k"; "kafka"] ~docv:"KAFKA_CLUSTER" ~doc)

type cluster_info = {
  kafka_host: string;
  partition: int;
  partition_count: int;
}

let consume_topic cluster topic =
  let host = cluster.kafka_host in
  let partition = cluster.partition in
  KafkaStore.Source.kafka_partition ~host ~topic ~partition

let produce_topic cluster topic =
  let host = cluster.kafka_host in
  Dataset.log (KafkaStore.Sink.log_to_topic ~host ~topic)

let store_kyoto_plain =
  let encode_key = id in
  let decode_key = id in
  let encode_value = id in
  let decode_value = id in
  fun file_path ->
    KyotoCabinet.KVStore.store ~encode_key ~decode_key ~encode_value ~decode_value ~file_path
    |> Dataset.persist_state

let store_kyoto_string_int =
  let encode_key = id in
  let decode_key = id in
  let encode_value = string_of_int in
  let decode_value = int_of_string in
  fun file_path ->
    KyotoCabinet.KVStore.store ~encode_key ~decode_key ~encode_value ~decode_value ~file_path
    |> Dataset.persist_state

let most_recent =      (* FIXME: should take the most recent, not the last provided value *)
  let open Reducer in
  {                                                                                                                                                                                               
    seed = "";
    push = (fun x _ -> x);
    term = id;
    full_check = None;
  }

let decode_position_messages cluster =
  consume_topic cluster "passover.positions.messages"
  |> Dataset.map position_of_json
  |> Dataset.filter_map Option.of_result
  |> Dataset.map json_of_position
  |> Dataset.filter_map Option.of_result
  |> produce_topic cluster "passover.positions.events"  (* FIXME: should assign a partition *)
  |> Dataset.run "/tmp/" "decode_position_messages"

let update_current_visitor_room cluster =
  consume_topic cluster "passover.positions.events"
  |> Dataset.map position_of_json
  |> Dataset.filter_map Option.of_result
  |> Dataset.group visitor_of_event room_of_event most_recent
  |> store_kyoto_plain "visitor_room.kct" 
  |> Dataset.run "/tmp/" "update_current_visitor_room"

let runner kafka_host partition partition_count =
  let cluster =
    { kafka_host; partition; partition_count; }
  in
  Lwt_main.run (Lwt.join [
    decode_position_messages cluster;
    update_current_visitor_room cluster;
  ])

let runner_t =
  Term.(pure runner $ kafka_host $ partition $ partition_count)

let main () =
  match Term.eval (runner_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_unix.with_async_detach main
