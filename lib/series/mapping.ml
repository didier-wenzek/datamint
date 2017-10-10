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
  | Base: ('k,'v) base -> ('k,'v) t
  | View: ('k,'v,'w) view -> ('k,'w) t

let new_hashtbl () =
  Hashtbl.create 1024

module Base = struct
  let hash_get m k =
    try Some (Hashtbl.find m k)
    with Not_found -> None

  let is_removed m = Hashtbl.mem m.removed
  let is_replaced m = Hashtbl.mem m.replaced
  let replaced_value m = hash_get m.replaced
  let source_value m = m.source.value
  let is_updated_key m k =
    (is_removed m k) || (is_replaced m k)
  let has_updated_key m (k,_) =
    is_updated_key m k 

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

  let rec source_of_hashtbl tbl =
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
      source_of_hashtbl tbl
    in
    Bounded.reduce Reducer.{
      seed; push; term; full_check = None;
    }

  let new_empty_source () = {
    keys = Bounded.empty;
    pairs = Bounded.empty;
    value = (fun k -> None);
    update = (fun updates -> hash_update (new_hashtbl ()) updates);
  }

  let of_source src = {
    source = src;
    replaced = new_hashtbl ();
    removed = new_hashtbl ();
  }

  let of_hashtbl tbl = of_source (source_of_hashtbl tbl)

  let of_pairs kvs =
    let tbl = new_hashtbl () in
    let seed = () in
    let push (k,v) () = Hashtbl.replace tbl k v in
    let term () = of_hashtbl tbl in
    kvs |> Bounded.reduce Reducer.{
      seed; push; term; full_check = None;
    }

  let do_replace (k,v) m =
    Hashtbl.remove m.removed k;
    Hashtbl.replace m.replaced k v

  let do_remove k m =
    Hashtbl.remove m.replaced k;
    Hashtbl.replace m.removed k ()

  let do_update_with key init push v m = 
    let k = key v in
    let acc = match value m k with
      | None -> init ()
      | Some acc -> acc
    in
    do_replace (k, push v acc) m

  let replace kv m =
    do_replace kv m;
    m

  let remove k m =
    do_remove k m;
    m

  let update_with key init push kv m = 
    do_update_with key init push kv m;
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
    let source = m.source.update (updates m) in
    let () = Hashtbl.clear m.replaced in
    let () = Hashtbl.clear m.removed in
    { m with source }

  let rollback_updates m =
    let () = Hashtbl.clear m.replaced in
    let () = Hashtbl.clear m.removed in
    m

  let of_new_hashtbl () =
    new_hashtbl ()
    |> of_hashtbl
   
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

  let cow v = Base.of_pairs (pairs v)
end

let of_source src = Base (Base.of_source src )
let of_hashtbl tbl = Base (Base.of_hashtbl tbl)
let of_pairs kvs = Base (Base.of_pairs kvs)
let new_empty () = of_source (Base.new_empty_source ())

let value = function
  | Base b -> Base.value b
  | View v -> View.value v

let keys = function
  | Base b -> Base.keys b
  | View v -> View.keys v

let pairs = function
  | Base b -> Base.pairs b
  | View v -> View.pairs v

let values m =
  let get_value = value m in
  fun k -> Bounded.of_option (get_value k)

let project f = function
  | Base b -> View (View.of_base b f)
  | View v -> View (View.project f v)

let updates = function
  | Base b -> Base.updates b
  | View v -> View.updates v

let replace kv = function
  | Base b -> Base (Base.replace kv b)
  | View v -> Base (Base.replace kv (View.cow v))

let remove k = function
  | Base b -> Base (Base.remove k b)
  | View v -> Base (Base.remove k (View.cow v))

let update_with key init push kv = function
  | Base b -> Base (Base.update_with key init push kv b)
  | View v -> Base (Base.update_with key init push kv (View.cow v))

let apply_updates updates = function
  | Base b -> Base (Base.apply_updates updates b)
  | View v -> Base (Base.apply_updates updates (View.cow v))

let commit_updates = function
  | Base b -> Base (Base.commit_updates b)
  | View b -> View (View.commit_updates b)

let rollback_updates = function
  | Base b -> Base (Base.rollback_updates b)
  | View b -> View (View.rollback_updates b)

let group key value v_red =
  let seed = new_empty () in
  let init () = v_red.Reducer.seed in
  let push = update_with key init (fun pair -> v_red.Reducer.push (value pair)) in
  let term = project v_red.Reducer.term in
  Reducer.{
    seed; push; term; full_check = None;
  }

let group_updates extract_key extract_value red insert rem =
  let open Reducer in
  let view = red.term in
  let adapt_push push x (kvs,acc) =
    let k = extract_key x in
    let v = extract_value x in
    let v0, acc = match value kvs k with
      | None -> red.seed, acc
      | Some v0 -> v0, push (rem (k,view v0)) acc
    in
    let v1 = red.push v v0 in
    let acc = push (insert (k,view v1)) acc in
    let kvs = replace (k,v1) kvs in
    (kvs,acc)
  in
  let adapt_term push term (kvs,acc) =
    term acc
  in
  fun r -> {
    seed = (new_empty (), r.seed);
    push = adapt_push r.push;
    term = adapt_term r.push r.term;
    full_check = combine_full_checks None r;
  }
