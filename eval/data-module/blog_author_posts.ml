module DB = Blog_storage.Open(Blog_storage.EmptyDB)
module Bag = Blog_storage.Bag
module Blog = Blog_query.Compile(DB)
open Blog

let iter_rows f =
  Bag.fold () f (fun () () -> ())

let () =
  let q = posts_of_author "foo" in
  let r = run q in
  iter_rows (fun (_,uuid,title,date) -> Printf.printf "%s: %s (%d)\n" uuid title date) r
