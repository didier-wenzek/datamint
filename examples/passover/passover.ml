open Message
open Series
open Util
module Dataset = Incremental_value

let host = "localhost"

let consume_topic topic partition =
  KafkaStore.Source.kafka_partition ~host ~topic ~partition

let produce_topic topic =
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

let decode_position_messages partition =
  consume_topic "passover.positions.messages" partition
  |> Dataset.map position_of_json
  |> Dataset.filter_map Option.of_result
  |> Dataset.map json_of_position
  |> Dataset.filter_map Option.of_result
  |> produce_topic "passover.positions.events"  (* FIXME: should assign a partition *)
  |> Dataset.run "/tmp/" "decode_position_messages"

let update_current_visitor_room partition =
  consume_topic "passover.positions.events" partition
  |> Dataset.map position_of_json
  |> Dataset.filter_map Option.of_result
  |> Dataset.group visitor_of_event room_of_event most_recent
  |> store_kyoto_plain "visitor_room.kct" 
  |> Dataset.run "/tmp/" "update_current_visitor_room"

let main () =
  let partition = 1 in
  Lwt_main.run (Lwt.join [
    decode_position_messages partition;
    update_current_visitor_room partition;
  ])

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_unix.with_async_detach main
