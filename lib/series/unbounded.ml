open Lwt.Infix
open Util

type 'a step = Done of 'a | Continue of 'a

module Step = struct
  let map f = function
    | Done a -> Done (f a)
    | Continue a -> Continue (f a)

  let map_lwt f = function
    | Done a -> f a >|= fun b -> Done b
    | Continue a -> f a >|= fun b -> Continue b
end

type ('a,'b) generator = {
  seed: 'a;
  next: 'a -> 'a step Lwt.t;
  view: 'a -> 'b Lwt.t;
}

type ('a,'s) producer = { generator: 'c 'd. ('a,'c,'d) Reducer.t -> ('s*'c,'d) generator }

type ('i,'e,'a) source = {
  next_chunk: 'e -> ('i * 'a) Bounded.producer Lwt.t;
  index_reducer: ('i,'e,'e) Reducer.t;
}

(* A product of two reducers,
   where the first reducer computes the extent of the input accumulated by the second reducer.

   The key point difference is how termination is defined.
   An indexed-reducing computation is done when
   *either* all the inputs have been consumed (i.e. the extent is full)
   *or* if the accumulated value has reach a maximum (i.e. the accumulation is full).
*)
let indexed_reducer r s =
  let open Reducer in
  let full_if_either_is = match r.full_check, s.full_check with
    | Some r_full, Some s_full -> Some (fun (xs,ys) -> r_full xs || s_full ys)
    | Some r_full, None -> Some (fun (xs,_) -> r_full xs)
    | None, Some s_full -> Some (fun (_,ys) -> s_full ys)
    | None, None -> None
  in {
    (product r s) with full_check = full_if_either_is;
  }

let of_source xs =
  { generator = fun red ->
      let open Reducer in
      let idx_red = indexed_reducer xs.index_reducer { red with term = id } in
      let continue_unless_done = match idx_red.full_check with
        | None -> fun acc -> Continue acc
        | Some full -> fun acc -> if full acc then Done acc else Continue acc
      in
      {
        seed = idx_red.seed;
        next = (fun (ext,acc) ->
          xs.next_chunk ext
          >|= fun chunk ->
          continue_unless_done (Bounded.iter idx_red chunk (ext,acc))
        );
        view = (fun (_,acc) -> Lwt.return (red.term acc));
      }
  }

let generate xs = xs.generator

let reduce r xs =
  let gen = xs.generator r in
  let step state =
    gen.next state
    >>= function
    | Done state -> Lwt.return Process.(Done state)
    | Continue new_state -> (
      gen.view new_state
      >>= fun _ ->
      Lwt.return Process.(Continue new_state)
    )
  in
  Process.loop_until_done_or_interrupted step gen.seed
  >>= fun last_state ->
  gen.view Process.(step_value last_state) 

let view f gen =
  { gen with
    view = (fun s -> gen.view s >|= f)
  }

let of_bounded =
  let open Bounded in
  let open Reducer in
  fun xs -> {
    generator = fun red -> {
      seed = (false, red.seed);
      next = (
        fun (done_status, acc) ->
          if done_status then Lwt.return (Done(done_status, acc))
          else Lwt.return (Done(true, xs.iter red))
      );
      view = (fun (_, acc) -> Lwt.return (red.term acc));
    }
  }

type ('a, 'b) either = Left of 'a | Right of 'b

let append xs ys =
  { generator = fun red ->
    let xs_gen = xs.generator red in
    let ys_gen = ys.generator red in
    let (xs_seed, red_seed) = xs_gen.seed in
    let (ys_seed, _) = ys_gen.seed in
    let next_xs s =
      xs_gen.next s
      >|= function
      | Continue (s,acc) -> Continue (Left s, acc)
      | Done (s,acc) -> Continue (Right ys_seed, acc)
    in
    let next_ys s =
      ys_gen.next s
      >|= function
      | Continue (s,acc) -> Continue (Right s, acc)
      | Done (s,acc) -> Done (Right s, acc)
    in
    {
      seed = (Left xs_seed, red_seed);
      next = (fun (s,acc) ->
        match s with
        | Left s -> next_xs (s,acc)
        | Right s -> next_ys (s,acc)
      );
      view = (fun (_, acc) -> Lwt.return (red.Reducer.term acc));
    }
  }

let bounded_generator ?(batch_size = 1024) seed next is_done =
  let iter red =
    let push = red.Reducer.push in
    let check_is_done = match red.Reducer.full_check with
      | None ->
        fun s -> Continue s
      | Some full_check ->
        fun s ->
          if full_check (snd s) then Done s
          else Continue s
    in
    let rec loop i (s,acc) =
      if i >= batch_size then (s,acc)
      else
        let (s,x) = next s in
        loop (i+1) (s, push x acc)
    in
    loop 0 >> check_is_done >> Lwt.return
  in
  { generator = fun red -> {
    seed = (seed,red.Reducer.seed);
    next = iter red;
    view = (fun (_, acc) -> Lwt.return (red.Reducer.term acc));
  }}

let generator ?(batch_size = 1024) seed next =
  bounded_generator ~batch_size seed next (fun _ -> false)

let iterate ?(batch_size = 1024) f x =
  generator ~batch_size x (fun x -> (f x,x))

let integers = iterate succ 0

let map f xs = { generator = fun r -> xs.generator (Reducer.map f r) }
let filter f xs = { generator = fun r -> xs.generator (Reducer.filter f r) }
let filter_map f xs = { generator = fun r -> xs.generator (Reducer.filter_map f r) }
let flat_map f xs = { generator = fun r -> xs.generator (Reducer.flat_map Bounded.iter f r) }
let unnest f xs = { generator = fun r -> xs.generator (Reducer.unnest Bounded.iter f r) }

let empty = of_bounded Bounded.empty
let singleton x = of_bounded Bounded.(singleton x)
let sonc xs x = append xs (singleton x)
let cons x xs = append (singleton x) xs

let hoist_inner_state =
  let move_inner_state_left (outer_state,(inner_state,acc)) = ((outer_state,inner_state),acc) in
  let move_inner_state_right ((outer_state,inner_state),acc) = (outer_state,(inner_state,acc)) in
  fun gen -> {
    seed = move_inner_state_left gen.seed ;
    next = (fun s -> move_inner_state_right s |> gen.next >|= Step.map (move_inner_state_left));
    view = move_inner_state_right >> gen.view;
  }

let take n xs = { generator = fun r -> xs.generator (Reducer.take n r) |> hoist_inner_state }
let take_while p xs = { generator = fun r -> xs.generator (Reducer.take_while p r) |> hoist_inner_state }
let take_first xs = take 1 xs
let take_last xs = { generator = fun r -> xs.generator (Reducer.take_last r) |> hoist_inner_state }
let drop n xs = { generator = fun r -> xs.generator (Reducer.drop n r) |> hoist_inner_state }
let drop_while p xs = { generator = fun r -> xs.generator (Reducer.drop_while p r) |> hoist_inner_state }
let rolling red xs = { generator = fun r -> xs.generator (Reducer.rolling red r) |> hoist_inner_state }
let interpose sep xs = { generator = fun r -> xs.generator (Reducer.interpose sep r) |> hoist_inner_state }
let before_each sep xs = { generator = fun r -> xs.generator (Reducer.before_each sep r) }
let after_each sep xs = { generator = fun r -> xs.generator (Reducer.after_each sep r) }
let dedupe xs = { generator = fun r -> xs.generator (Reducer.dedupe r) |> hoist_inner_state }
let unique xs = { generator = fun r -> xs.generator (Reducer.unique r) |> hoist_inner_state }

let decorate fst lst xs =
  append (singleton fst) (append xs (singleton lst))

let group_updates key value red insert remove xs =
  { generator = fun r -> xs.generator (Mapping.group_updates key value red insert remove r) |> hoist_inner_state }

