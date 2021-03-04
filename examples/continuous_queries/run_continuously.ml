open Series
open Util
module Dataset = Incremental_value

let sum = Reducer.commutative_monoid 0 (+)
let count xs = Dataset.reduce (Reducer.map (constant 1) sum) xs
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
  let log = Store.file_logger id "integers.log" in
  Dataset.of_unbounded Unbounded.integers
  |> Dataset.map string_of_int
  |> Dataset.log log
  |> Dataset.run "/tmp/" "query_0"
  
let loop1 () =
  let log = Store.file_logger id "kafka_topic_test.log" in
  KafkaStore.Source.kafka_partition ~host:"localhost" ~topic:"test" ~partition:1
  |> Dataset.log log 
  |> Dataset.run "/tmp/" "query_1"
  
let loop2 () =
  let file = Store.file_view string_of_int "kafka_topic_test.count" in
  KafkaStore.Source.kafka_partition ~host:"localhost" ~topic:"test" ~partition:1
  |> Dataset.dedupe
  |> count
  |> Dataset.persist_view file 
  |> Dataset.run "/tmp/" "query_2"

let loop2' () =
  let file = Store.file_view string_of_int "kafka_topic_test.unique_count" in
  KafkaStore.Source.kafka_partition ~host:"localhost" ~topic:"test" ~partition:1
  |> Dataset.unique
  |> count
  |> Dataset.persist_view file 
  |> Dataset.run "/tmp/" "query_2_prime"

let kyoto_string_int_store =
  let encode_key = id in
  let decode_key = id in
  let encode_value = string_of_int in
  let decode_value = int_of_string in
  fun file_path ->
    KyotoCabinet.KVStore.store ~encode_key ~decode_key ~encode_value ~decode_value ~file_path

let loop3 () =
  let count = Reducer.map (constant 1) sum in
  let store = kyoto_string_int_store "word_count.kct" in
  KafkaStore.Source.kafka_partition ~host:"localhost" ~topic:"test" ~partition:1
  |> Dataset.flat_map words
  |> Dataset.group id id count
  |> Dataset.persist_state store 
  |> Dataset.run "/tmp/" "query_3"
  
let loop4 () =
  let log = KafkaStore.Sink.log_to_topic ~host:"localhost" ~topic:"spam" in
  KafkaStore.Source.kafka_partition ~host:"localhost" ~topic:"test" ~partition:1
  |> Dataset.filter spam
  |> Dataset.log log 
  |> Dataset.run "/tmp/" "query_4"

let loop5 () =
  let count = Reducer.map (constant 1) sum in
  let log = Store.file_logger id "word_count_updates.log" in
  let store = kyoto_string_int_store "word_count_looking_new_words.kct" in
  let insert (w,c) = Format.sprintf "new word with %d samples: %s" c w in
  let remove (w,c) = Format.sprintf "word %s has now more than %d samples" w c in
  KafkaStore.Source.kafka_partition ~host:"localhost" ~topic:"test" ~partition:1
  |> Dataset.flat_map words 
  |> Dataset.group_updates id id count insert remove
  |> Dataset.persist_state store 
  |> Dataset.log log
  |> Dataset.run "/tmp/" "query_5"

let loop6 () =
  let count = Reducer.map (constant 1) sum in
  let store_count = kyoto_string_int_store "top_word_count_state.kct" in
  let file = Store.file_view id "top_word_count.txt" in
  let insert wc = Some wc in
  let remove _wc = None in
  KafkaStore.Source.kafka_partition ~host:"localhost" ~topic:"test" ~partition:1
  |> Dataset.flat_map words 
  |> Dataset.group_updates id id count insert remove
  |> Dataset.persist_state store_count
  |> Dataset.flat_map Bounded.of_option
  |> Dataset.reduce (Top.reducer fst snd 10)
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

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  main ()
