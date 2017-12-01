open Lwt.Infix
open Series
open Util

type kafka_partition = {
  handler: Kafka.handler;
  topic: Kafka.topic;
  partition: Kafka.partition;
  mutable started: bool;
  mutable msg_count: int;
  mutable timeout_ms: int;
}

let timeout_ms = 100
let msg_count = 1024
let max_msg_count = 16*1024
let max_timeout_ms = 1000
let min_msg_count = 1
let min_timeout_ms = 0

let partition_offset_reducer = Reducer.{
    seed = Kafka.offset_beginning;
    push = max;
    term = id;
    full_check = None;
  }

let connect_partition kafka_cluster topic_name partition =
  let kafka_options = ["metadata.broker.list",kafka_cluster] in
  let topic_options = [] in
  let handler = Kafka.new_consumer kafka_options in
  let topic = Kafka.new_topic handler topic_name topic_options in
  { handler; topic; partition; started = false; msg_count; timeout_ms }

let init_partition_connection con offset =
  if not con.started
  then begin
    let next_offset = if offset < 0L then offset else Int64.succ offset in
    Kafka.consume_start con.topic con.partition next_offset;
    con.started <- true                                                                                                                                                                                  
  end

let extract_payload = function
  | Kafka.Message (_, _, offset, msg, _) -> Some (offset,msg)
  | Kafka.PartitionEnd (_, _, _) -> None

let reduce_buffer con =
  con.msg_count <- max min_msg_count (con.msg_count / 2);
  con.timeout_ms <- min max_timeout_ms (con.timeout_ms * 2)

let enlarge_buffer con =
  con.msg_count <- min max_msg_count (con.msg_count * 2);
  con.timeout_ms <- max min_timeout_ms (con.timeout_ms / 2)

let adjust_buffer_size con received =
  if received < con.msg_count / 2
  then
    reduce_buffer con
  else
    if received = con.msg_count
    then
      enlarge_buffer con

let consume_batch con =
  let timeout_ms = con.timeout_ms in
  let msg_count = con.msg_count in
  Kafka_lwt.consume_batch ~timeout_ms ~msg_count con.topic con.partition
  >>= fun messages ->
  let () = adjust_buffer_size con (List.length messages) in
  let messages = Bounded.(filter_map extract_payload (of_list messages)) in
  Lwt.return messages

let next_messages_from_partition con offset =
  init_partition_connection con offset;
  consume_batch con

let source_of_partition ~host ~topic ~partition =
  let con = connect_partition host topic partition in
  Unbounded.{
    next_chunk = next_messages_from_partition con;
    index_reducer = partition_offset_reducer;
  }

let messages_of_partition ~host ~topic ~partition =
  Unbounded.of_source (source_of_partition ~host ~topic ~partition)

let kafka_partition ~host ~topic ~partition =
  Incremental_value.of_unbounded (messages_of_partition host topic partition)
