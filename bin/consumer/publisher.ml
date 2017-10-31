open Sexplib.Std

type resource = string [@@deriving sexp]

type publisher_kind =
  | Kafka of string
  [@@deriving sexp]

type config = resource * publisher_kind
  [@@deriving sexp]
