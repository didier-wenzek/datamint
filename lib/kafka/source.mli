(* Read messages from a given kafka partition *)
val messages_of_partition: host:string -> topic:string -> partition:int -> (string, int64) Series.Unbounded.producer

(* Incremental stream of messages read from a given kafka partition *)
val kafka_partition: host:string -> topic:string -> partition:int -> (string, int64) Series.Incremental_value.producer

(* Unbounded source of messages publish on a given kafka partition *)
val source_of_partition: host:string -> topic:string -> partition:int -> (int64, int64, string) Series.Unbounded.source
