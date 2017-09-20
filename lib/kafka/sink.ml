open DataMint.Series

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
    List.iter write (List.rev buff);
    Lwt.return_unit
  in
  Store.Logger {
    init_buffer;
    push_buffer;
    write_buffer;
  }

let log_to_partition ~host ~topic ~partition =
  let handler, topic = connect_topic host topic in
  fun ignore_dir_path -> buffered_logger (Kafka.produce topic partition)

let log_to_topic ~host ~topic =
  let partition = Kafka.partition_unassigned in
  log_to_partition ~host ~topic ~partition
