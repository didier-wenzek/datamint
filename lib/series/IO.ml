open Util

let iter_dir =
  let next_entry dir =
    try Some (Unix.readdir dir)
    with End_of_file -> None
  in
  let iter_entries push path acc dir =
    let rec loop acc =
      match next_entry dir with
      | None -> acc
      | Some name -> 
        if name = "." || name = ".."
        then loop acc
        else loop (push (Filename.concat path name) acc)
    in
    loop acc
  in
  fun push path acc ->
    with_resource Unix.opendir Unix.closedir path (iter_entries push path acc)

let is_readable path =
  try close_in (open_in path); true
  with Sys_error _ -> false

let iter_files push =
  let rec loop path acc = Unix.(
    match (lstat path).st_kind with
    | S_REG when is_readable path -> push path acc
    | S_DIR -> (
      try iter_dir loop path acc
      with Unix_error(EACCES,_,_) -> acc
    )
    | _ -> acc
  ) in
  loop

let files path =
  Bounded.(producer_of_source {
    fold = fun push -> iter_files push path
  })

let iter_file_chunks =
  let iter_chunks size push acc channel =
    let buffer = Bytes.create size in
    let rec loop acc =
      let l = input channel buffer 0 size in
      if l = 0 then acc
      else
        loop (push (Bytes.sub_string buffer 0 l) acc)
    in
    loop acc
  in
  fun size push path acc ->
  with_resource open_in close_in path (iter_chunks size push acc)

let file_chunks size path =
  Bounded.(producer_of_source {
    fold = fun push -> iter_file_chunks size push path
  })

type 'a split =
  | NoSplit of 'a
  | NoMiddle of 'a * 'a
  | Split of 'a * 'a * 'a

let split_first_last char chunk =
  match String.index chunk char, String.rindex chunk char with
  | exception Not_found ->
    NoSplit chunk
  | i, j when i = j -> 
    let n = String.length chunk in
    NoMiddle (String.sub chunk 0 i, String.sub chunk (j+1) (n-j-1))
  | i, j ->
    let n = String.length chunk in
    Split (String.sub chunk 0 i, String.sub chunk (i+1) (j-i-1), String.sub chunk (j+1) (n-j-1))

let iter_chunk char push chunk =
  let rec loop i acc =
    match String.index_from chunk i char with
    | exception Not_found ->
      let n = String.length chunk in
      push (String.sub chunk i (n-i)) acc
    | j ->
      let acc = push (String.sub chunk i (j-i)) acc in
      loop (j+1) acc
  in
  loop 0

let file_lines path =
  let init () = Buffer.create 1024 in
  let adapt push_line chunk (left_over, acc) =
    match split_first_last '\n' chunk with
    | NoSplit chunk ->
      let () = Buffer.add_string left_over chunk in
      (left_over, acc)
    | NoMiddle (head,tail) ->
      let () = Buffer.add_string left_over head in
      let head = Buffer.contents left_over in
      let () = Buffer.clear left_over in
      let () = Buffer.add_string left_over tail in
      let acc = push_line head acc in
      (left_over, acc)
    | Split (head, middle,tail) ->
      let () = Buffer.add_string left_over head in
      let head = Buffer.contents left_over in
      let () = Buffer.clear left_over in
      let () = Buffer.add_string left_over tail in
      let acc = push_line head acc in
      let acc = iter_chunk '\n' push_line middle acc in
      (left_over, acc)
  in
  let term push (left_over, state) =
    if Buffer.length left_over = 0 then state
    else push (Buffer.contents left_over) state
  in
  let pack_chunks r () = Reducer.(
    stateful_adapter init adapt term None r
  ) in
  Bounded.{ iter = fun r -> iter_and_term (pack_chunks { r with Reducer.term = id } ()) (file_chunks (8*1024) path) }
