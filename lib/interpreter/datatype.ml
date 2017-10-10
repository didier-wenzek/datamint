type name = string
  [@@ deriving show]

type t =
  | Atomic of name
  | Tuple of t list 
  | Record of (name * t) list
  | List of t
  [@@ deriving show]
