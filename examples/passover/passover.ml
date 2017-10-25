open Message
open Series
open Util
module Dataset = Incremental_value

let host = "localhost"

let position_messages_topic partition =
  let topic = "passover.positions.messages" in
  KafkaStore.Source.kafka_partition ~host ~topic ~partition

let position_topic =
  let topic = "passover.positions.events" in
  KafkaStore.Sink.log_to_topic ~host ~topic

let decode_position_messages partition =
  position_messages_topic partition
  |> Dataset.map position_of_json
  |> Dataset.filter_map Option.of_result
  |> Dataset.map json_of_position
  |> Dataset.filter_map Option.of_result
  |> Dataset.log position_topic
  |> Dataset.run "/tmp/" "decode_position_messages"

let main () =
  let partition = 1 in
  Lwt_main.run (Lwt.join [
    decode_position_messages partition;
  ])

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_unix.with_async_detach main
