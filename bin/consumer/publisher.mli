type resource = string

type publisher_kind =
  | Kafka of string

type config = resource * publisher_kind
  [@@deriving sexp]
