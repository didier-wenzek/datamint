open Cmdliner
open Message
open Series
open Util
open Lwt.Infix
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
  Cmd.info "passover" ~doc ~man

let partition =
  let doc = "partition this instance will consume (for all Kafka topics)" in
  Arg.(required & pos 0 (some int) None & info [] ~docv:"PARTITION" ~doc)

let partition_count =
  let doc = "count of partitions (for all Kafka topics)" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"PARTITION_COUNT" ~doc)

let kafka_host =
  let doc = "Connection string to the Kafka cluster (comma separated list of hostnames or hostname:port pairs) ." in
  Arg.(value & opt string "localhost" & info ["k"; "kafka"] ~docv:"KAFKA_CLUSTER" ~doc)

let working_dir =
  let doc = "Directory where all data and states are stored." in
  Arg.(value & opt string "/tmp/passover" & info ["d"; "data"] ~docv:"WORKING_DIR" ~doc)

type cluster_info = {
  kafka_host: string;
  working_dir: string;
  partition: int;
  partition_count: int;
}

module Topic = struct
  let positions_messages = "passover-positions-messages"
  let positions_events = "passover-positions-events"
  let visitors_moves = "passover-visitors-moves"
  let rooms_visitorcounts = "passover-rooms-visitorcounts"
end

let consume_topic cluster topic =
  let host = cluster.kafka_host in
  let partition = cluster.partition in
  KafkaStore.Source.kafka_partition ~host ~topic ~partition

let produce_topic cluster topic key encode =
  let host = cluster.kafka_host in
  Dataset.log (KafkaStore.Sink.log_to_topic_partition ~host ~topic ~key ~encode)

let prepare cluster =
  Lwt.catch
    (fun () ->
      Lwt_unix.mkdir cluster.working_dir 0o750)
    (function
      | Unix.Unix_error(Unix.EEXIST,_,_) -> Lwt.return_unit
      | e -> Lwt.fail e)

let add_process_id cluster path =
  let base = Filename.remove_extension path in
  let ext = Filename.extension path in
  let id = cluster.partition in
  let count = cluster.partition_count in
  Printf.sprintf "%s.%d-%d%s" base id count ext

let add_working_dir cluster path =
  if Filename.is_relative path
  then Filename.concat cluster.working_dir path
  else path

let trace_errors xs =
  Dataset.filter_map (function
    | Ok x -> Some x
    | Error e ->
      Printf.eprintf "ERROR: %s\n%!" e;
      None
  ) xs

let run cluster name =
  let dir = cluster.working_dir in
  let name = add_process_id cluster name in
  Dataset.run dir name

let store_kyoto_plain cluster =
  let encode_key = id in
  let decode_key = id in
  let encode_value = id in
  let decode_value = id in
  fun file_path ->
    let file_path = file_path |> add_process_id cluster in
    KyotoCabinet.KVStore.store ~encode_key ~decode_key ~encode_value ~decode_value ~file_path
    |> Dataset.persist_state

let store_kyoto_int cluster =
  let encode_key = id in
  let decode_key = id in
  let encode_value = string_of_int in
  let decode_value = int_of_string in
  fun file_path ->
    let file_path = file_path |> add_process_id cluster in
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

let visitor_of_move = function
  | Insert (v,_) | Remove (v,_) -> v

let room_of_move = function
  | Insert (_,r) | Remove (_,r) -> r

let count_update_of_move = function
  | Insert (_,_) -> 1
  | Remove (_,_) -> -1


let filter_insert = function
  | Insert i -> Some i
  | _ -> None

let key_of_count (r,_) = r

let string_of_count (r,c) =
  Printf.sprintf "{ \"room\": \"%s\", \"count\": %d }" r c

let decode_position_messages cluster =
  consume_topic cluster Topic.positions_messages
  |> Dataset.map position_of_json
  |> trace_errors
  |> produce_topic cluster Topic.positions_events visitor_of_event json_of_position
  |> run cluster "decode_position_messages"

let update_current_visitor_room cluster =
  consume_topic cluster Topic.positions_events
  |> Dataset.map position_of_json
  |> Dataset.filter_map Option.of_result
  |> Dataset.group_updates visitor_of_event room_of_event most_recent insert remove
  |> store_kyoto_plain cluster "visitor_room.kct" 
  |> produce_topic cluster Topic.visitors_moves room_of_move string_of_move
  |> run cluster "update_current_visitor_room"

let count_room_visitors cluster =
  consume_topic cluster Topic.visitors_moves
  |> Dataset.map move_of_string
  |> trace_errors
  |> Dataset.group_updates room_of_move count_update_of_move Reducer.sum insert remove
  |> store_kyoto_int cluster "room_visitor_count.kct" 
  |> Dataset.filter_map filter_insert
  |> produce_topic cluster Topic.rooms_visitorcounts key_of_count string_of_count
  |> run cluster "count_room_visitors"

let runner kafka_host working_dir partition partition_count =
  let cluster =
    { kafka_host; working_dir; partition; partition_count; }
  in
  Lwt_main.run (
    prepare cluster >>= fun () ->
    Lwt.join [
      decode_position_messages cluster;
      update_current_visitor_room cluster;
      count_room_visitors cluster;
  ])

let runner_t =
  Term.(const runner $ kafka_host $ working_dir $ partition $ partition_count)

let main () =
  Cmd.v info runner_t |> Cmd.eval |> exit

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  main ()
