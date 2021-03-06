let connect_topic kafka_cluster topic_name =
  let kafka_options = ["metadata.broker.list",kafka_cluster] in
  let topic_options = [] in
  let handler = Kafka.new_producer kafka_options in
  let topic = Kafka.new_topic handler topic_name topic_options in
  (handler, topic)

let log_event topic _resource event =
  Lwt.wrap (fun () -> Kafka.produce topic Kafka.partition_unassigned event)

let topic_logger kafka_cluster topic =
  let handler, topic = connect_topic kafka_cluster topic in
  log_event topic
