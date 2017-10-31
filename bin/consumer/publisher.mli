type resource = string

type publisher_kind =
  | Kafka of string

type config = resource * publisher_kind
  [@@deriving sexp]

type publisher = (string -> unit Lwt.t) -> unit Lwt.t

val make_publisher: publisher_kind -> publisher Lwt.t
