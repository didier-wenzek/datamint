(* Log messages to a given kafka partition *)
val log_to_partition: host:string -> topic:string -> partition:int -> string Series.Store.log

(* Log messages to a given kafka topic (using round-robin over the topic's partitions) *)
val log_to_topic: host:string -> topic:string -> string Series.Store.log

(* Log messages to a given kafka topic, using the given functions to assign a partition and to encode the message. *)
val log_to_topic_partition: host:string -> topic:string -> key: ('a -> 'b) -> encode: ('a -> string) -> 'a Series.Store.log
