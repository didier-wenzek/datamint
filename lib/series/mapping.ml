open Util

type 'a producer = 'a Bounded.producer 
type ('a,'b,'c) reducer = ('a,'b,'c) Reducer.t

type ('k,'v) update =
  | Replace of 'k*'v
  | Remove of 'k

type ('k,'v) source = {
  keys: 'k producer;
  pairs: ('k * 'v) producer;
  value: 'k -> 'v option;
  update: ('k,'v) update producer -> ('k,'v) source;
}

type ('k,'v) base = {
  source: ('k,'v) source;
  replaced: ('k,'v) Hashtbl.t;
  removed: ('k,unit) Hashtbl.t;
}

type ('k,'v,'w) view = {
  base: ('k,'v) base;
  project: 'v -> 'w;
}

type ('k,'v) t =
  | Source: ('k,'v) source -> ('k,'v) t
  | Base: ('k,'v) base -> ('k,'v) t
  | View: ('k,'v,'w) view -> ('k,'w) t

let new_hashtbl () =
  Hashtbl.create 1024

let hash_keys m =
  let fold push =
    let push k v = push k in
    Hashtbl.fold push m
  in
  Bounded.producer_of_source { Bounded.fold }

let hash_pairs m =
  let fold push =
    let push k v = push (k,v) in
    Hashtbl.fold push m
  in
  Bounded.producer_of_source { Bounded.fold }

let hash_get m k =
  try Some (Hashtbl.find m k)
  with Not_found -> None

module Source = struct
  let keys m = m.keys
  let pairs m = m.pairs
  let value m k = m.value k

  let rec of_hashtbl tbl =
    {
      keys = hash_keys tbl;
      pairs = hash_pairs tbl;
      value = hash_get tbl;
      update = hash_update tbl;
    }

  and hash_update tbl =
    let seed = () in
    let push u () =
      match u with
      | Replace (k,v) -> Hashtbl.replace tbl k v
      | Remove k -> Hashtbl.remove tbl k
    in
    let term () =
      of_hashtbl tbl
    in
    Bounded.reduce Reducer.{
      seed; push; term; full_check = None;
    }

  let empty = {
    keys = Bounded.empty;
    pairs = Bounded.empty;
    value = (fun k -> None);
    update = (fun updates -> hash_update (new_hashtbl ()) updates);
  }

  let of_pairs kvs =
    let tbl = new_hashtbl () in
    let seed = () in
    let push (k,v) () = Hashtbl.replace tbl k v in
    let term () = of_hashtbl tbl in
    kvs |> Bounded.reduce Reducer.{
      seed; push; term; full_check = None;
    }

end

module Base = struct
  let is_removed m = Hashtbl.mem m.removed
  let is_replaced m = Hashtbl.mem m.replaced
  let replaced_value m = hash_get m.replaced
  let source_value m = m.source.value
  let is_updated_key m k =
    (is_removed m k) || (is_replaced m k)
  let has_updated_key m (k,_) =
    is_updated_key m k 

  let value m k =
    if is_removed m k
    then None
    else
      match replaced_value m k with
      | None -> source_value m k
      | v -> v

  let keys m =
    Bounded.append
      (hash_keys m.replaced)
      (m.source.keys |> Bounded.remove (is_updated_key m))

  let pairs m =
    Bounded.append
      (hash_pairs m.replaced)
      (m.source.pairs |> Bounded.remove (has_updated_key m))

  let of_source src = {
    source = src;
    replaced = new_hashtbl ();
    removed = new_hashtbl ();
  }

  let do_replace (k,v) m =
    Hashtbl.remove m.removed k;
    Hashtbl.replace m.replaced k v

  let do_remove k m =
    Hashtbl.remove m.replaced k;
    Hashtbl.replace m.removed k ()

  let do_update_with key v_seed push v m = 
    let k = key v in
    let acc = match value m k with
      | None -> v_seed
      | Some acc -> acc
    in
    do_replace (k, push v acc) m

  let replace kv m =
    do_replace kv m;
    m

  let remove k m =
    do_remove k m;
    m

  let update_with key v_seed push kv m = 
    do_update_with key v_seed push kv m;
    m

  let updates m =
    Bounded.append
      (hash_keys m.removed |> Bounded.map (fun k -> Remove k))
      (hash_pairs m.replaced |> Bounded.map (fun (k,v) -> Replace (k,v)))

  let apply_update_reducer m =
    let seed = () in
    let push u () =
      match u with
      | Replace (k,v) -> do_replace (k,v) m
      | Remove k -> do_remove k m
    in
    let term () = m in
    Reducer.{
      seed; push; term; full_check = None;
    }

  let apply_updates updates m =
    Bounded.reduce (apply_update_reducer m) updates

  let commit_updates m =
    let updates = updates m in
    if Bounded.is_empty updates
    then m
    else 
      let source = m.source.update updates in
      let () = Hashtbl.clear m.replaced in
      let () = Hashtbl.clear m.removed in
      { m with source }

  let rollback_updates m =
    let () = Hashtbl.clear m.replaced in
    let () = Hashtbl.clear m.removed in
    m

end

module View = struct
  let value v = Base.value v.base >> Option.map v.project
  let keys v = Base.keys v.base
  let pairs v = Base.pairs v.base |> Bounded.map (fun (k,a) -> (k, v.project a))

  let updates v =
    let project_updates = function
      | Replace(k,a) -> Replace (k, v.project a)
      | Remove k -> Remove k  (* we need to rewrite the update event to avoid type unification : type a ~ type v.project a *)
    in
    Base.updates v.base |> Bounded.map project_updates

  let commit_updates v =
    let base = Base.commit_updates v.base in
    { v with base }

  let rollback_updates v =
    let base = Base.rollback_updates v.base in
    { v with base }

  let project f v = {
    v with
    project = v.project >> f
  }

  let of_base b f = {
    base = b;
    project = f;
  }

  let cow v = Base.of_source (Source.of_pairs (pairs v))
end

let empty = Source (Source.empty)
let of_source src = Source (src)
let of_hashtbl tbl = Source (Source.of_hashtbl tbl)
let of_pairs kvs = Source (Source.of_pairs kvs)

let value = function
  | Source s -> Source.value s
  | Base b -> Base.value b
  | View v -> View.value v

let keys = function
  | Source s -> Source.keys s
  | Base b -> Base.keys b
  | View v -> View.keys v

let pairs = function
  | Source s -> Source.pairs s
  | Base b -> Base.pairs b
  | View v -> View.pairs v

let values m =
  let get_value = value m in
  fun k -> Bounded.of_option (get_value k)

let project f = function
  | Source s -> View (View.of_base (Base.of_source s) f)
  | Base b -> View (View.of_base b f)
  | View v -> View (View.project f v)

let updates = function
  | Source s -> Base.updates (Base.of_source s)
  | Base b -> Base.updates b
  | View v -> View.updates v

let replace kv = function
  | Source s -> Base (Base.replace kv (Base.of_source s))
  | Base b -> Base (Base.replace kv b)
  | View v -> Base (Base.replace kv (View.cow v))

let remove k = function
  | Source s -> Base (Base.remove k (Base.of_source s))
  | Base b -> Base (Base.remove k b)
  | View v -> Base (Base.remove k (View.cow v))

let update_with key v_seed push kv = function
  | Source s -> Base (Base.update_with key v_seed push kv (Base.of_source s))
  | Base b -> Base (Base.update_with key v_seed push kv b)
  | View v -> Base (Base.update_with key v_seed push kv (View.cow v))

let apply_updates updates = function
  | Source s -> Base (Base.apply_updates updates (Base.of_source s))
  | Base b -> Base (Base.apply_updates updates b)
  | View v -> Base (Base.apply_updates updates (View.cow v))

let commit_updates = function
  | Source s -> Base (Base.commit_updates (Base.of_source s))
  | Base b -> Base (Base.commit_updates b)
  | View b -> View (View.commit_updates b)

let rollback_updates = function
  | Source s -> Base (Base.rollback_updates (Base.of_source s))
  | Base b -> Base (Base.rollback_updates b)
  | View b -> View (View.rollback_updates b)

let group key value v_red =
  let seed = empty in
  let v_seed = v_red.Reducer.seed in
  let push = update_with key v_seed (fun pair -> v_red.Reducer.push (value pair)) in
  let term = project v_red.Reducer.term in
  Reducer.{
    seed; push; term; full_check = None;
  }

let group_updates extract_key extract_value red insert rem =
  let open Reducer in
  let view = red.term in
  let adapt_push push kv (kvs,acc) =
    let k = extract_key kv in
    let v = extract_value kv in
    let v0, is_new = match value kvs k with
      | None -> red.seed, true
      | Some v0 -> v0, false
    in
    let v1 = red.push v v0 in
    if v1 = v0
    then
      (kvs,acc)
    else
      let kvs = replace (k,v1) kvs in
      let acc =
        if is_new then acc
        else push (rem (k,view v0)) acc
      in
      let acc = push (insert (k,view v1)) acc in
      (kvs,acc)
  in
  let adapt_term push term (kvs,acc) =
    term acc
  in
  fun r -> {
    seed = (empty, r.seed);
    push = adapt_push r.push;
    term = adapt_term r.push r.term;
    full_check = combine_full_checks None r;
  }

(*
   # Map equality

   forall (k, k_xs, k_ys) in
       { k -> (some @ x, ∅) | (k,x) in xs }
     & { k -> (∅, some @ y) | (k,y) in ys }
   then
     k_xs == k_ys

   # Set equality

   forall (x, in_xs, in_ys) in
       { x -> (exists @ x, ∅) | x in xs }
     & { x -> (∅, exists @ y) | y in ys }
   then
     in_xs == in_ys

   # MultiMaps equality

   forall (k, k_xs, k_ys) in
       { k -> (set @ x, ∅) | (k,x) in xs }
     & { k -> (∅, set @ y) | (k,y) in ys }
   then
     k_xs == k_ys
*)

let left x = (Some x, None)
let right x = (None, Some x)

let matching_pairs eq =
  let first x y = match x with
    | None -> y
    | x -> x
  in
  let merge_pairs (x1,y1) (x2,y2) =
    (first x1 x2, first y1 y2)
  in
  let full_pair = function
    | (Some _, Some _) -> true
    | _ -> false
  in
  let check_pair_eq = function
    | (Some x, Some y) -> eq x y
    | _ -> false
  in
  Reducer.{
    seed = (None,None);
    push = merge_pairs;
    term = check_pair_eq;
    full_check = Some full_pair
  }

let equal key_eq value_eq =
  let group_matching_pairs =
    Bounded.reduce (group fst snd (matching_pairs value_eq))
  in
  fun m n ->
    let m_pairs = pairs m |> Bounded.map (fun (k,v) -> (k, left v)) in
    let n_pairs = pairs n |> Bounded.map (fun (k,v) -> (k, right v)) in
    let matches = Bounded.append m_pairs n_pairs |> group_matching_pairs in
    pairs matches |> Bounded.forall snd
