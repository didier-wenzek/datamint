open Bounded

let split sep s =
  of_list (Str.split (Str.regexp sep) s)

let extract word =
  let word = Str.regexp word in
  let next s i =
    try
      let j = Str.search_forward word s i in
      let w = Str.matched_string s in
      Some (j + String.length w, w)
    with
      Not_found -> None
  in
  fun s -> producer_of_generator {
    seed = 0;
    next = next s;
  }
