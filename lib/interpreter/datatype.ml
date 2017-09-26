type name = string

type t =
  | Atomic of name
  | Tuple of t list 
  | Record of (name * t) list
  | List of t

let rec show = function
  | Atomic name -> name
  | Tuple [] -> "()"
  | Tuple [t] -> show t
  | Tuple (s::t) -> "("^ (show_tuple s t) ^")"
  | Record [] -> "{}"
  | Record (f::r) -> "{"^ (show_record f r) ^"}"
  | List l -> "[" ^ (show l) ^ "]"

and show_tuple a = function
  | [] -> show a
  | s::t -> (show a)^", "^(show_tuple s t)

and show_record (f,s) = function
  | [] ->   f ^ ": " ^ (show s)
  | n::r -> f ^ ": " ^ (show s) ^", "^(show_record n r)
