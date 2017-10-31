open Sexplib.Std

type resource = string [@@deriving sexp]

type publisher_kind =
  | Kafka of string
  [@@deriving sexp]

type config = resource * publisher_kind
  [@@deriving sexp]

type publisher = (string -> unit Lwt.t) -> unit Lwt.t

let make_publisher = function
  | Kafka topic -> Lwt.return (Kafka_publisher.topic_publisher topic)
