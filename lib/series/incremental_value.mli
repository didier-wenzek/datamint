type 'a view = 'a Store.view
type 'a log = 'a Store.log
type 'a store =
  | GenStore : 'a store
  | Store: 'a Store.store -> 'a store
  | PairGen: 'a store -> ('a * 'b) store
  | PairStore: ('a store * 'b Store.store) -> ('a * 'b) store

type monotonic
type non_monotonic

(* An incremental value of type [('k,'a,'s) t] represents a continuous computation
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

val run: Store.path -> Store.path -> ('k,'a,'s) t -> unit Lwt.t

val log: 'a log -> ('a,'s) producer -> ('a,'s) producer
val persist_view: 'a view -> ('a,'s) aggregate -> ('a,'s) aggregate
val persist_full_state: 's store -> ('k,'a,'s) t -> ('k,'a,'s) t
val persist_state: 'b Store.store -> ('k,'a,'s * 'b) t -> ('k,'a,'s * 'b) t

val of_bounded: 'a Bounded.producer -> ('a,bool) producer
val of_unbounded: ('a,'s) Unbounded.producer -> ('a,'s) producer

val reduce: ('a,'b,'c) Reducer.t -> ('a,'s) producer -> ('c,'s * 'b) aggregate
val rolling: ('a,'b,'c) Reducer.t -> ('a,'s) producer -> ('c,'s * 'b) producer

val view: ('a -> 'b) -> ('a,'s) aggregate -> ('b,'s) aggregate
val reduce_view: ('a,'b,'c) Reducer.t -> ('a,'s) collection -> ('c,'s) aggregate

val map: ('a -> 'b) -> ('a,'s) producer -> ('b,'s) producer
val filter: ('a -> bool) -> ('a,'s) producer -> ('a,'s) producer
val filter_map: ('a -> 'a option) -> ('a,'s) producer -> ('a,'s) producer
val flat_map: ('a -> 'b Bounded.producer) -> ('a,'s) producer -> ('b,'s) producer
val unnest: ('a -> 'b Bounded.producer) -> ('a,'s) producer -> ('b*'a,'s) producer

val map_view: ('a -> 'b) -> ('a,'s) collection -> ('b,'s) collection
val filter_view: ('a -> bool) -> ('a,'s) collection -> ('a,'s) collection
val flat_map_view: ('a -> 'b Bounded.producer) -> ('a,'s) collection -> ('b,'s) collection

val first: ('a,'s) producer -> ('a option, 's * 'a option) aggregate
val last: ('a,'s) producer -> ('a option, 's * 'a option) aggregate
val take: int -> ('a,'s) producer -> ('a, 's * int) producer
val take_while: ('a -> bool) -> ('a,'s) producer -> ('a, 's * bool) producer
val drop: int -> ('a,'s) producer -> ('a, 's * int) producer
val drop_while: ('a -> bool) -> ('a,'s) producer -> ('a, 's * bool) producer

val interpose: 'a -> ('a,'s) producer -> ('a,'s * bool) producer
val before_each: 'a -> ('a,'s) producer -> ('a,'s) producer
val after_each: 'a -> ('a,'s) producer -> ('a,'s) producer

val dedupe: ('a,'s) producer -> ('a,'s * 'a option) producer
val unique: ('a,'s) producer -> ('a,'s * ('a, unit) Hashtbl.t option) producer

val group:
     ('a -> 'k)                                        (* extract the grouping key *)
  -> ('a -> 'v)                                        (* extract the value to be grouped *)
  -> ('v,'m,'u) Reducer.t                              (* reduce the values *)
  -> ('a,'s) producer                                  (* input stream *)
  -> (('k,'u) Mapping.t,'s * ('k,'m) Mapping.t) aggregate (* output mapping *)

val group_updates:
     ('a -> 'k)                                        (* extract the grouping key *)
  -> ('a -> 'v)                                        (* extract the value to be grouped *)
  -> ('v,'m,'u) Reducer.t                              (* reduce the values *)
  -> ('k*'u -> 'c)                                     (* output to emit in case of insertion *)
  -> ('k*'u -> 'c)                                     (* output to emit in case of deletion *)
  -> ('a,'s) producer                                  (* input stream *)
  -> ('c,'s * ('k,'m) Mapping.t) producer              (* output stream *)
