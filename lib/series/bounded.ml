open Util

type 'a producer = { iter: 'b 'c.  ('a,'b,'c) Reducer.t -> 'b }
type 'a source = { fold: 'b. ('a -> 'b -> 'b) -> 'b -> 'b }
type ('s,'a) generator = { seed: 's; next: 's -> ('s*'a) option }

let reduce r xs =
  let open Reducer in
  match r.full_check with
  | None -> r.term (xs.iter r)
  | Some full_check ->
    with_return (fun return ->
      let push x s = if full_check s then return (r.term s) else r.push x s in
      r.term (xs.iter { r with push })
    )

let fold push seed =
  let open Reducer in
  reduce {
    seed;
    push;
    term = id;
    full_check = None;
  }

let producer_of_source xs =
  let open Reducer in
  let iter r = 
    match r.full_check with
    | None ->
      xs.fold r.push (r.seed)
    | Some full_check ->
      with_return (fun return ->
        let push x s = if full_check s then return s else r.push x s in
        xs.fold push (r.seed)
      )
  in
  { iter }

let producer_of_generator xs =
  let fold push =
    let rec loop state acc =
      match xs.next state with
      | None -> acc
      | Some (state, x) -> push x acc |> loop state
    in
    loop xs.seed
  in
  producer_of_source { fold }

let generator seed next =
  producer_of_generator { seed; next = fun s -> Some (next s) }

let bounded_generator seed next is_done =
  let next s =
    if is_done s then None else Some (next s)
  in
  producer_of_generator { seed; next; }

let iter r xs seed = xs.iter Reducer.{ r with seed}
let iter_and_term r xs = r.Reducer.term (xs.iter r)

let union_reducer red =
  let open Reducer in
  {
    red with
    push = iter red 
  }

let transduce seed step term done_check =
  let adapt r =
    let open Reducer in
    let push x (s,acc) =
      let (s,ys) = step x s in
      let acc = ys.iter { r with seed = acc } in
      (s, acc)
    in
    let term (s,acc) =
      let ys = term s in
      ys.iter { r with seed = acc }
    in
    { seed = combine_seeds seed r.seed;
      push;
      term;
      full_check = combine_full_checks done_check r;
    }
  in
  fun xs -> { iter = fun r -> iter_and_term (adapt r) xs }

let map f xs = { iter = fun r -> xs.iter (Reducer.map f r) }
let filter f xs = { iter = fun r -> xs.iter (Reducer.filter f r) }
let remove f xs = { iter = fun r -> xs.iter (Reducer.remove f r) }
let filter_map f xs = { iter = fun r -> xs.iter (Reducer.filter_map f r) }
let flat_map f xs = { iter = fun r -> xs.iter (Reducer.flat_map iter f r) }
let unnest f xs = { iter = fun r -> xs.iter (Reducer.unnest iter f r) }

let map_fst f xs = { iter = fun r -> xs.iter (Reducer.map_fst f r) }
let map_snd f xs = { iter = fun r -> xs.iter (Reducer.map_snd f r) }
let filter_fst f xs = { iter = fun r -> xs.iter (Reducer.filter_fst f r) }
let filter_snd f xs = { iter = fun r -> xs.iter (Reducer.filter_snd f r) }
let filter_map_fst f xs = { iter = fun r -> xs.iter (Reducer.filter_map_fst f r) }
let filter_map_snd f xs = { iter = fun r -> xs.iter (Reducer.filter_map_snd f r) }

let forall p xs = xs |> map p |> reduce Reducer.and_reducer
let exists p xs = xs |> map p |> reduce Reducer.or_reducer
let is_empty xs = forall (fun _ -> false) xs
let length xs = xs |> map (fun _ -> 1) |> reduce Reducer.sum

let empty = { iter = fun r -> Reducer.(r.seed) }
let singleton x = { iter = fun r -> Reducer.(r.seed |> r.push x) }
let sonc xs x = { iter = fun r -> Reducer.(xs.iter r |> r.push x) }
let cons x xs = { iter = fun r -> Reducer.(r.seed |> r.push x |> iter r xs) }
let append xs ys = { iter = fun r -> Reducer.(xs.iter r |> iter r ys) }
let concat xss = { iter = fun r -> xss.iter Reducer.{ r with push = iter r } }

let take n xs = { iter = fun r -> iter_and_term Reducer.(take n { r with term = id }) xs }
let take_while p xs = { iter = fun r -> iter_and_term Reducer.(take_while p { r with term = id }) xs }
let take_first xs = take 1 xs
let take_last xs = { iter = fun r -> iter_and_term Reducer.(take_last { r with term = id }) xs }
let drop n xs = { iter = fun r -> iter_and_term Reducer.(drop n { r with term = id }) xs }
let drop_while p xs = { iter = fun r -> iter_and_term Reducer.(drop_while p { r with term = id }) xs }
let rolling red xs = { iter = fun r -> iter_and_term Reducer.(rolling red { r with term = id }) xs }
let interpose sep xs = { iter = fun r -> iter_and_term Reducer.(interpose sep { r with term = id }) xs }
let before_each sep xs = { iter = fun r -> xs.iter (Reducer.before_each sep r) }
let after_each sep xs = { iter = fun r -> xs.iter (Reducer.after_each sep r) }
let decorate fst lst xs = { iter = fun r -> Reducer.(r.seed |> r.push fst |> iter r xs |> r.push lst) }
let dedupe xs = { iter = fun r -> iter_and_term Reducer.(dedupe { r with term = id }) xs }
let unique xs = { iter = fun r -> iter_and_term Reducer.(unique { r with term = id }) xs }

let show fst sep lst show_item items =
  items |> map show_item |> interpose sep |> decorate fst lst |> reduce Reducer.string_reducer

let take_no_more_than n dots =
  let step x i = if i<n then (i+1,singleton x) else (n+1,empty) in
  let term i = if i>n then singleton dots else empty in
  let done_check i  = i > n in
  transduce 0 step term (Some done_check)

let show_first n fst sep dots lst show_item items =
  items |> map show_item |> take_no_more_than n dots |> show fst sep lst id

let repeat n x =
  let fold push =
    let rec loop i acc =
      if i > n then acc
      else loop (i+1) (push x acc)
    in loop 1
  in
  producer_of_source { fold }

let iterate n f x =
  let fold push =
    let rec loop i x acc =
      if i > n then acc
      else loop (i+1) (f x) (push x acc)
    in loop 1 x
  in
  producer_of_source { fold }

let range min max =
  let fold push =
    let rec loop i acc =
      if i > max then acc
      else loop (i+1) (push i acc)
    in loop min
  in
  producer_of_source { fold }

let of_list xs =
  let fold push =
    let rec loop xs acc = match xs with
      | [] -> acc
      | x::xs -> loop xs (push x acc)
    in
    loop xs
  in
  producer_of_source { fold }

let of_option = function
  | None -> empty
  | Some x -> singleton x

let of_hashtbl xs =
  let fold push =
    let push k v = push (k,v) in
    Hashtbl.fold push xs
  in
  producer_of_source { fold }

let to_list xs = reduce Reducer.list_reducer xs

let to_bag xs = reduce Reducer.bag_reducer xs

(** Equality of two series.

   forall (i, ox, oy) in
       { i -> (option @ x, ø) | (i,x) in xs }
     & { i -> (ø, option @ y) | (i,y) in ys }
   then
     ox == oy
*)
let equal item_eq =
  let push y = function
    | (true, x::xs) when item_eq x y -> (true, xs)
    | _ -> (false, [])
  in
  let is_full = function
    | (true, _) -> false
    | _ -> true
  in
  let term = function
    | (true, []) -> true
    | _ -> false
  in
  fun xs ->
    let xs = to_list xs in
    let seed = (true,xs) in
    let check_items =
      Reducer.{
        seed;
        push; term;
        full_check = Some is_full;
      }
    in
    reduce check_items
