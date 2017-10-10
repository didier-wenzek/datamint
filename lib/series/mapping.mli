type 'a producer = 'a Bounded.producer 
type ('a,'b,'c) reducer = ('a,'b,'c) Reducer.t

(* Atomic update of a mapping. *)
type ('k,'v) update =
  | Replace of 'k*'v
  | Remove of 'k

(* A primary mapping content as provided by a data source *)
type ('k,'v) source = {
  keys: 'k producer;
  pairs: ('k * 'v) producer;
  value: 'k -> 'v option;
  update: ('k,'v) update producer -> ('k,'v) source;
}

(* A mapping under construction *)
type ('k,'v) base = {
  source: ('k,'v) source;
  replaced: ('k,'v) Hashtbl.t;
  removed: ('k,unit) Hashtbl.t;
}

(* The projection of a mapping *)
type ('k,'v,'w) view = {
  base: ('k,'v) base;
  project: 'v -> 'w;
}

(* The type of mapping from a key to a singular optional value *)
type ('k,'v) t =
  | Source: ('k,'v) source -> ('k,'v) t
  | Base: ('k,'v) base -> ('k,'v) t
  | View: ('k,'v,'w) view -> ('k,'w) t

val empty: ('k,'v) t
val of_source: ('k,'v) source -> ('k,'v) t
val of_hashtbl: ('k,'v) Hashtbl.t -> ('k,'v) t 
val of_pairs: ('k*'v) producer -> ('k,'v) t

val project: ('v -> 'w) -> ('k,'v) t -> ('k,'w) t

val keys: ('k,'v) t -> 'k producer
val pairs: ('k,'v) t -> ('k * 'v) producer
val value: ('k,'v) t -> 'k -> 'v option
val values: ('k,'v) t -> 'k -> 'v producer

val replace: ('k*'v) -> ('k,'v) t -> ('k,'v) t
val remove: 'k -> ('k,'v) t -> ('k,'v) t
val update_with: ('a -> 'k) -> 'v -> ('a -> 'v -> 'v) -> 'a -> ('k,'v) t -> ('k,'v) t

val updates: ('k,'v) t -> ('k,'v) update producer
val apply_updates: ('k,'v) update producer -> ('k,'v) t -> ('k,'v) t

val commit_updates: ('k,'v) t -> ('k,'v) t
val rollback_updates: ('k,'v) t -> ('k,'v) t

val group:
     ('a -> 'k)                          (* extract the grouping key *)
  -> ('a -> 'v)                          (* extract the value to be grouped *)
  -> ('v,'m,'w) reducer                  (* reduce the grouped values *)
  -> ('a, ('k,'m) t, ('k,'w) t) reducer  (* a reducer of `a` values into a mapping from key to reduced values *)

val group_updates:
     ('a -> 'k)                          (* extract the grouping key *)
  -> ('a -> 'v)                          (* extract the value to be grouped *)
  -> ('v,'m,'w) reducer                  (* reduce the grouped values *)
  -> ('k*'w -> 'b)                       (* command to emit when a pair is inserted *)
  -> ('k*'w -> 'b)                       (* command to emit when a pair is removed *)
  -> ('b,'c,'d) reducer                  (* command reducer *)
  -> ('a, ('k, 'm) t * 'c, 'd) reducer   (* a reducer of `a` values into a stream of commands *)
