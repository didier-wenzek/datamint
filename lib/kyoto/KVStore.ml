open Series
open Util

let with_db path = Kyoto.with_db path [Kyoto.OREADER; Kyoto.ONOLOCK] 

let iter_pairs path push seed =
  let push_pair acc kv = push kv acc in
  let fold db = Kyoto.fold db push_pair seed in
  with_db path fold

let all_pairs path =
  Bounded.(producer_of_source {
    fold = (fun push -> iter_pairs path push)
  })

let iter_keys path push seed =
  let push_key acc (k,_) = push k acc in
  let fold db = Kyoto.fold db push_key seed in
  with_db path fold

let all_keys path =
  Bounded.(producer_of_source {
    fold = (fun push -> iter_keys path push)
  })

let get_value path key =
  with_db path (fun db -> Kyoto.get db key)

let get_values path key =
  with_db path (fun db -> Kyoto.get db key |> Bounded.of_option)

let iter_pairs_with_prefix path prefix push seed =
  let push_pair acc kv = push kv acc in
  let fold db = Kyoto.fold_prefix db prefix push_pair seed in
  with_db path fold

let pairs_with_prefix path prefix =
  Bounded.(producer_of_source {
    fold = (fun push -> iter_pairs_with_prefix path prefix push)
  })

let iter_pairs_within_range path min max push seed =
  let push_pair acc kv = push kv acc in
  let fold db = Kyoto.fold_range db min max push_pair seed in
  with_db path fold

let pairs_within_range path min max =
  Bounded.(producer_of_source {
    fold = (fun push -> iter_pairs_within_range path min max push)
  })

let update_reducer encode_key encode_value path =
  let init () =
    let db = Kyoto.opendb path Kyoto.[OWRITER;OCREATE] in
    Kyoto.begin_tran db;
    db
  in
  let push u db = match u with
    | Mapping.Replace (k,v) ->
      Kyoto.set db (encode_key k) (encode_value v); db
    | Mapping.Remove k ->
      Kyoto.remove db (encode_key k); db
  in
  let term = Kyoto.commit_tran in
  let full_check = None in
  Reducer.of_buffer ~init ~term ~push ~full_check

let update encode_key encode_value path =
  Bounded.reduce (update_reducer encode_key encode_value path)

let mapping encode_key decode_key encode_value decode_value path =
  let open Mapping in
  let decode_pair (k,v) = (decode_key k, decode_value v) in
  let rec src =
    {
      keys = (all_keys path |> Bounded.map decode_key);
      pairs = (all_pairs path |> Bounded.map decode_pair);
      value = (encode_key >> get_value path >> Option.map decode_value);
      update = (fun updates -> update encode_key encode_value path updates; src);
    }
  in
  of_source src

let store ~encode_key ~decode_key ~encode_value ~decode_value ~file_path dir_path =
  let path = Filename.concat dir_path file_path in
  let read_content _ =
    mapping encode_key decode_key encode_value decode_value path
    |> Lwt.return
  in
  let write_content mapping =
    Mapping.commit_updates mapping
    |> Lwt.return
  in
  Store.{ read_content; write_content; }
