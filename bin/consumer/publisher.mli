type resource = string

type publisher_kind =
  | Kafka of string

type config = resource * publisher_kind
  [@@deriving sexp]

type publisher

val make_publisher: publisher_kind -> publisher Lwt.t
