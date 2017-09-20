open Series.Util
module Reducer = Series.Reducer
module Unbounded = Series.Unbounded
module Bounded = Series.Bounded

(*
module Dataset = Incremental_value_bis
module Store = Store_bis

let sum = Reducer.commutative_monoid 0 (+)
let count = Reducer.map (constant 1) sum
let words =
  let sep = Str.regexp "[ \t]+" in
  let split = Str.split sep in
  fun str -> Bounded.of_list (split str)

let spam =
  let clue = Str.regexp "[xX]+" in
  fun str ->
    try
      let _ = Str.search_forward clue str 0 in
      true
    with Not_found -> false


let loop0 () =
  let log = Store_bis.file_logger id "integers.log" in
  Dataset.of_unbounded Unbounded.integers
  |> Dataset.map string_of_int
  |> Dataset.log log
  |> Dataset.run "/tmp/" "query_0"
  
let loop1 () =
  let log = Store_bis.file_logger id "kafka_topic_test.log" in
  Dataset.of_unbounded (Kafka_source.kafka_partition "localhost" "test" 1)
  |> Dataset.log log 
  |> Dataset.run "/tmp/" "query_1"
  
let loop2 () =
  let file = Store_bis.file_view string_of_int "kafka_topic_test.count" in
  Dataset.of_unbounded (Kafka_source.kafka_partition "localhost" "test" 1)
  |> Dataset.dedupe
  |> Dataset.reduce count
  |> Dataset.persist_view file 
  |> Dataset.run "/tmp/" "query_2"

let loop2' () =
  let file = Store_bis.file_view string_of_int "kafka_topic_test.unique_count" in
  Dataset.of_unbounded (Kafka_source.kafka_partition "localhost" "test" 1)
  |> Dataset.unique
  |> Dataset.reduce count
  |> Dataset.persist_view file 
  |> Dataset.run "/tmp/" "query_2_prime"

let loop3 () =
  let open Store in
  let store = Sift_kyoto.store_bis id id string_of_int int_of_string "word_count.kct" in
  Dataset.of_unbounded (Kafka_source.kafka_partition "localhost" "test" 1)
  |> Dataset.flat_map words
  |> Dataset.group id id count
  |> Dataset.persist_state store 
  |> Dataset.run "/tmp/" "query_3"
  
let loop4 () =
  let open Store in
  let log = Kafka_store.log_to_topic "localhost" "spam" in
  Dataset.of_unbounded (Kafka_source.kafka_partition "localhost" "test" 1)
  |> Dataset.filter spam
  |> Dataset.log log 
  |> Dataset.run "/tmp/" "query_4"

let loop5 () =
  let log = Store_bis.file_logger id "word_count_updates.log" in
  let store = Sift_kyoto.store_bis id id string_of_int int_of_string "word_count_looking_new_words.kct" in
  let insert (w,c) = Format.sprintf "new word with %d samples: %s" c w in
  let remove (w,c) = Format.sprintf "word %s has now more than %d samples" w c in
  Dataset.of_unbounded (Kafka_source.kafka_partition "localhost" "test" 1)
  |> Dataset.flat_map words 
  |> Dataset.group_updates id id count insert remove
  |> Dataset.persist_state store 
  |> Dataset.log log
  |> Dataset.run "/tmp/" "query_5"

let loop6 () =
  let store_count = Sift_kyoto.store_bis id id string_of_int int_of_string "top_word_count_state.kct" in
  let file = Store_bis.file_view id "top_word_count.txt" in
  let insert wc = Some wc in
  let remove wc = None in
  Dataset.of_unbounded (Kafka_source.kafka_partition "localhost" "test" 1)
  |> Dataset.flat_map words 
  |> Dataset.group_updates id id count insert remove
  |> Dataset.persist_state store_count
  |> Dataset.flat_map Bounded.of_option
  |> Dataset.reduce (Sift_top.reducer fst snd 10)
  |> Dataset.map_view (fun (w,c) -> Format.sprintf "%s: %d\n" w c)
  |> Dataset.reduce_view Reducer.string_reducer
  |> Dataset.persist_view file 
  |> Dataset.run "/tmp/" "query_6"

let main () =
  Lwt_main.run (Lwt.join [
    loop1 ();
    loop2 (); loop2' ();
    loop3 ();
    loop4 ();
    loop5 ();
    loop6 ();
  ])
*)

let main () = Lwt_main.run Lwt.return_unit

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_unix.with_async_detach main
