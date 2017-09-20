open Lwt.Infix

type 'a view = 'a Store.view
type 'a log = 'a Store.log
type 'a store =
  | GenStore : 'a store
  | Store: 'a Store.store -> 'a store
  | PairGen: 'a store -> ('a * 'b) store
  | PairStore: ('a store * 'b Store.store) -> ('a * 'b) store

type monotonic
type non_monotonic

(* An incremental value of type [('a,'s) t] represents a continuous computation
   producing an unbounded series of value of type ['a] using a state of type ['s].

   An incremental value is either an unbounded series
   or the result of a fold operation over an unbounded series (i.e. a monoid morphism).
*)
type ('k,'a,'s) t =
  | UProducer: ('a,'s) Unbounded.producer * 's store * 'a log option  -> (monotonic, 'a Bounded.producer, 's) t
  | Aggregate: ('s,'a) Unbounded.generator * 's store * 'a view option -> (non_monotonic, 'a, 's) t

type ('a,'s) producer = (monotonic, 'a Bounded.producer, 's) t
type ('a,'s) aggregate = (non_monotonic, 'a, 's) t
type ('a,'s) collection = (non_monotonic, 'a Bounded.producer, 's) t

let combined_store state_path =
  let rec loop: type a. a store -> a Store.store_combiner
    = let open Store in function
    | GenStore -> Combiner (marshal_store state_path, unit_store, take_left)
    | Store s -> Combiner (marshal_store state_path, s, take_right)
    | PairGen head ->
      let Combiner (_,head_store,head_combiner) = loop head in
      let pair_store = combine_store head_store unit_store in
      Combiner (marshal_store state_path, pair_store, take_left_tail head_combiner)
    | PairStore (head, tail_store) ->
      let Combiner (_,head_store,head_combiner) = loop head in
      let pair_store = combine_store head_store tail_store in
      Combiner (marshal_store state_path, pair_store, take_right_tail head_combiner)
  in
  loop

let store process_id s =
  Store.store_of_combiner (combined_store process_id s)

let run root_path process_id (type k) (type a) (type s): (k,a,s) t -> unit Lwt.t
   = function
  | UProducer (xs, store_shape, logger) ->
    let state_store = store process_id store_shape in
    let logger = match logger with
      | Some logger -> logger
      | None -> Store.console_logger (fun x -> "") (* FIXME *)
    in
    Store.log_unbounded_series state_store logger root_path xs

  | Aggregate (gen, store_shape, view_store) ->
    let state_store = store process_id store_shape in
    Store.store_unbounded_gen state_store view_store root_path gen

let log log = function
  | UProducer (xs, state_store, None) ->
    UProducer (xs, state_store, Some log)

  | UProducer (xs, state_store, Some former_log) ->
    UProducer (xs, state_store, Some (Store.combine_log former_log log))

let persist_view view = function
  | Aggregate (xs, state_store, None) ->
    Aggregate (xs, state_store, Some view)

  | Aggregate (xs, state_store, Some former_view) ->
    Aggregate (xs, state_store, Some (Store.combine_view former_view view))

let persist_full_state (type k) (type a) (type s) (state_store: s store): (k,a,s) t -> (k,a,s) t
  = function
  | UProducer (xs, _, view_store) ->
    UProducer (xs, state_store, view_store)
  | Aggregate (xs, _, view_store) ->
    Aggregate (xs, state_store, view_store)

let persist_state (type k) (type a) (type s) (type b) (s: b Store.store): (k,a,s * b) t -> (k,a,s * b) t
  = function
  | UProducer (xs, PairGen(state_store), view_store) ->
    UProducer (xs, PairStore(state_store, s), view_store)

  | UProducer (xs, PairStore(state_store, _), view_store) ->
    UProducer (xs, PairStore(state_store, s), view_store)

  | Aggregate (xs, PairGen(state_store), view_store) ->
    Aggregate (xs, PairStore(state_store, s), view_store)

  | Aggregate (xs, PairStore(state_store, _), view_store) ->
    Aggregate (xs, PairStore(state_store, s), view_store)

  | _ -> raise (Invalid_argument "The inner state can only be changed when the state is compound.")

let of_unbounded xs = UProducer (xs, GenStore, None)
let of_bounded xs = of_unbounded (Unbounded.of_bounded xs)

let reduce red = function
  | UProducer (xs, state_store, view_store) ->
    let gen = Unbounded.generate xs red in
    Aggregate (gen, PairGen(state_store), None)

let rolling red = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.rolling red xs, PairGen(state_store), None)

let view f = function
  | Aggregate (gen, state_store, view_store) ->
    Aggregate (Unbounded.view f gen, state_store, None)

let reduce_view red = view (Bounded.reduce red)

let map f = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.map f xs, state_store, None)

let filter p = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.filter p xs, state_store, None)

let flat_map f = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.flat_map f xs, state_store, None)

let unnest f = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.unnest f xs, state_store, None)

let filter_map f = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.filter_map f xs, state_store, None)

let map_view f = view (Bounded.map f)
let filter_view f = view (Bounded.filter f)
let flat_map_view f = view (Bounded.flat_map f)

let first xs = reduce Reducer.first xs
let last xs = reduce Reducer.last xs

let take n = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.take n xs, PairGen(state_store), None)

let take_while p = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.take_while p xs, PairGen(state_store), None)

let drop n = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.drop n xs, PairGen(state_store), None)

let drop_while p = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.drop_while p xs, PairGen(state_store), None)

let interpose sep = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.interpose sep xs, PairGen(state_store), None)

let before_each sep = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.before_each sep xs, state_store, None)

let after_each sep = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.after_each sep xs, state_store, None)

let dedupe = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.dedupe xs, PairGen(state_store), None)

let unique = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.unique xs, PairGen(state_store), None)

let rolling red = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.rolling red xs, PairGen(state_store), None)

let group key value red =
  reduce (Mapping.group key value red)

let group_updates key value red insert remove = function
  | UProducer (xs, state_store, view_store) ->
    UProducer (Unbounded.group_updates key value red insert remove xs, PairGen(state_store), None)
