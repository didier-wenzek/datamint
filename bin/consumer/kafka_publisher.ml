open Lwt.Infix

let seed messages =
  let open Series.Unbounded in
  let open Series.Reducer in
  messages.index_reducer.seed

let publish_events messages push =
  let rec push_all = function
    | [] -> Lwt.return_unit
    | m::ms -> (
      push m >>= fun () ->
      push_all ms
    ) 
  in
  let push_all chunk =
    let open Series.Bounded in
    chunk |> map snd |> to_list |> push_all
  in
  let next_offset chunk =
    let open Series.Bounded in
    chunk |> map fst |> reduce messages.Series.Unbounded.index_reducer
  in
  let rec loop offset = 
    let open Series.Unbounded in
    messages.next_chunk offset
    >>= fun chunk ->
    push_all chunk
    >>= fun () ->
    loop (next_offset chunk)
  in
  loop (seed messages)

let publish_events messages partitions push =
  List.map (fun partition -> publish_events (messages partition) push) partitions
  |> Lwt.join 
  
let topic_publisher kafka_cluster topic =
  let partitions = [0;1;2] in
  let messages partition = KafkaStore.Source.source_of_partition kafka_cluster topic partition in
  publish_events messages partitions
