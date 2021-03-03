open Series

let connect_topic kafka_cluster topic_name =
  let kafka_options = ["metadata.broker.list",kafka_cluster] in
  let topic_options = [] in
  let handler = Kafka.new_producer kafka_options in
  let topic = Kafka.new_topic handler topic_name topic_options in
  (handler, topic)

let buffered_logger write =
  let init_buffer _ = [] in
  let push_buffer str buff = str::buff in
  let write_buffer buff =
    Lwt.wrap (fun () -> List.iter write (List.rev buff))
  in
  Store.Logger {
    init_buffer;
    push_buffer;
    write_buffer;
  }

let topic_partition_count handler topic =
  let open Kafka.Metadata in
  let meta = Kafka.topic_metadata handler topic in
  List.length meta.topic_partitions

let log_to_partition ~host ~topic ~partition =
  let _handler, topic = connect_topic host topic in
  fun _ignore_dir_path -> buffered_logger (Kafka.produce topic partition)

let log_to_topic ~host ~topic =
  let partition = Kafka.partition_unassigned in
  log_to_partition ~host ~topic ~partition

let log_to_topic_partition ~host ~topic ~key ~encode =
  let handler, topic = connect_topic host topic in
  let partition_count = topic_partition_count handler topic in
  let write x =
    let partition = (Hashtbl.hash (key x)) mod partition_count in
    let msg = encode x in
    Kafka.produce topic partition msg
  in
  fun _ignore_dir_path -> buffered_logger write
