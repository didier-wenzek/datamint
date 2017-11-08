open Util

type ('a,'b,'c) t = {
  seed: 'b;
  push: 'a -> 'b -> 'b;
  term: 'b -> 'c;
  full_check: ('b -> bool) option;
}

let map f r = { r with push = fun x -> r.push (f x) }
let filter f r  = { r with push = fun x -> if f x then r.push x else id }
let remove f r  = { r with push = fun x -> if f x then id else r.push x }
let filter_map f r  = { r with push = fun x -> match f x with Some y -> r.push y | None -> id }
let flat_map iter f r = { r with push = fun x -> iter r (f x) }
let unnest iter f r = { r with push = fun x -> iter { r with push = fun y -> r.push (y,x) } (f x) }
let project f r = { r with term = r.term >> f }

let map_fst f r = { r with push = fun (x,y) -> r.push (f x,y) }
let map_snd f r = { r with push = fun (x,y) -> r.push (x,f y) }
let filter_fst f = filter (fun (x,y) -> f x)
let filter_snd f = filter (fun (x,y) -> f y)
let filter_map_fst f r  = { r with push = fun (x,y) -> match f x with Some x' -> r.push (x',y) | None -> id }
let filter_map_snd f r  = { r with push = fun (x,y) -> match f y with Some y' -> r.push (x,y') | None -> id }

let monoid zero plus = {
  seed = zero;
  push = (fun x s -> plus s x);
  term = id;
  full_check = None;
}

let commutative_monoid zero plus = {
  seed = zero;
  push = plus;
  term = id;
  full_check = None;
}

let with_maximum_check check r = {
  r with 
  full_check = Some (check);
}

let with_maximum max = 
  with_maximum_check (fun s -> s = max)

let with_seed seed r = {
  r with seed
}

let monoid_with_maximum_check zero plus max =
  monoid zero plus |> with_maximum_check max

let monoid_with_maximum zero plus max =
  monoid zero plus |> with_maximum max

let product r s =
  let push (x,y) (xs,ys) = (r.push x xs, s.push y ys) in
  let term (xs,ys) = (r.term xs, s.term ys) in
  let full r_full s_full (xs,ys) = r_full xs && s_full ys in
  let full_check = match r.full_check, s.full_check with
    | Some r_full, Some s_full -> Some (full r_full s_full)
    | _ -> None
  in {
    seed = (r.seed, s.seed);
    push ;
    term ;
    full_check ;
  }

let of_buffer ~init ~push ~term ~full_check =
  let buffer = function
    | None -> init ()
    | Some buffer -> buffer
  in
  let push x acc =
    Some (push x (buffer acc))
  in
  let term acc =
    term (buffer acc)
  in
  let full_check = match full_check with
    | None -> None
    | Some check -> Some (function
      | None -> false
      | Some buffer -> check buffer
    )
  in {
    seed = None;
    push;
    term;
    full_check;
  }

let string_reducer =
  let init () = Buffer.create 80 in
  let push s b = Buffer.add_string b s; b in
  let term = Buffer.contents in
  let full_check = None in
  of_buffer ~init ~push ~term ~full_check

let combine_seeds s r = (s, r)

let combine_full_checks f r = match f,r.full_check with
  | Some f, Some g ->
    let check (i,j) = f i || g j in
    Some check
  | Some f, None ->
    let check (i,j) = f i in
    Some check
  | None, Some g ->
    let check (i,j) = g j in
    Some check
  | None, None ->
    None

let term_ignoring_state r (_,s) = r.term s

let state_adapter seed adapt_push adapt_term full_check r = 
  { seed = combine_seeds seed r.seed;
    push = adapt_push r.push;
    term = adapt_term r.push >> r.term;
    full_check = combine_full_checks full_check r;
  }

let stateful_adapter init adapt_push adapt_term full_check r = 
  let init () = (init (), r.seed) in
  let push = adapt_push r.push in
  let term = adapt_term r.push >> r.term in
  let full_check = combine_full_checks full_check r in
  of_buffer ~init ~push ~term ~full_check

let take n r =
  let push x (i,s) = if i < n then (i + 1, r.push x s) else (i,s) in
  let full_check i = i >= n in
  { seed = combine_seeds 0 r.seed;
    push;
    term = term_ignoring_state r;
    full_check = combine_full_checks (Some full_check) r;
  }

let take_while p r =
  let push x (ok,s) = if ok && p x then (true, r.push x s) else (false,s) in
  let full_check ok = not ok in
  { seed = combine_seeds true r.seed;
    push;
    term = term_ignoring_state r;
    full_check = combine_full_checks (Some full_check) r;
  }

let drop n r =
  let push x (i,s) = if i < n then (i+1,s) else (n, r.push x s) in
  { seed = combine_seeds 0 r.seed;
    push;
    term = term_ignoring_state r;
    full_check = combine_full_checks None r;
  }

let drop_while p r =
  let push x (ok,s) = if ok && p x then (true,s) else (false, r.push x s) in
  {
    seed = combine_seeds true r.seed;
    push;
    term = term_ignoring_state r;
    full_check = combine_full_checks None r;
  } 

let take_first r = take 1 r

let take_last r = 
  let push x (o,s) = (Some x,s) in
  let term = function
    | None,s -> r.term s
    | Some x, s -> r.term (r.push x s)
  in
  { seed = combine_seeds None r.seed;
    push;
    term;
    full_check = None;
  }

let rolling red r =
  let push x (acc,s) =
    let acc = red.push x acc in
    (acc, r.push (red.term acc) s)
  in
  { seed = combine_seeds red.seed r.seed;
    push;
    term = term_ignoring_state r;
    full_check = combine_full_checks None r;
  }

let interpose sep r =
  let push x (first,s) =
    if first
    then (false, r.push x s)
    else (false, r.push sep s |> r.push x)
  in
  { seed = combine_seeds true r.seed;
    push;
    term = term_ignoring_state r;
    full_check = combine_full_checks None r;
  }

let before_each sep r =
  let push x s =
    r.push sep s |> r.push x
  in
  { r with
    push;
  }

let after_each sep r =
  let push x s =
    r.push x s |> r.push sep
  in
  { r with
    push;
  }

let dedupe r =
  let push x previous_s =
    match previous_s with
    | (Some y, _) when y = x -> previous_s
    | (_, s) -> (Some x, r.push x s)
  in
  { seed = combine_seeds None r.seed;
    push;
    term = term_ignoring_state r;
    full_check = combine_full_checks None r;
  }

let first = {
  seed = None;
  push = (fun x -> function None -> Some x | xs -> xs);
  term = id;
  full_check = Some (function None -> false | _ -> true);
}

let last = {
  seed = None;
  push = (fun x _ -> Some x);
  term = id;
  full_check = None;
}

let first_or seed = {
  first with
  term = (function None -> seed | Some s -> s);
}

let last_or seed = {
  seed;
  push = (fun x _ -> x);
  term = id;
  full_check = None;
}

let and_reducer = commutative_monoid true (&&) |> with_maximum false
let or_reducer = commutative_monoid false (||) |> with_maximum true

let bag_reducer = {
  seed = [];
  push = (fun x xs -> x::xs);
  term = id;
  full_check = None;
}

let list_reducer = {
  bag_reducer with
  term = List.rev
}

let hash_reducer =
  let init () = Hashtbl.create 1024 in
  let push (k,v) xs = Hashtbl.add xs k v; xs in
  let buffer = function
    | None -> init ()
    | Some buffer -> buffer
  in
  let push x acc =
    Some (push x (buffer acc))
  in
  let term acc =
    (buffer acc)
  in {
    seed = None;
    push;
    term;
    full_check = None;
  }

let unique r =
  let insert x xs = Hashtbl.add xs x (); xs in
  let push x = function
    | (None, s) ->
      let xs = Hashtbl.create 1024 in
      (Some (insert x xs), r.push x s)
    | (Some xs, s) as xss when Hashtbl.mem xs x ->
      xss
    | (Some xs, s) ->
      (Some (insert x xs), r.push x s)
  in
  let term (_, s) =
    r.term s
  in {
    seed = (None, r.seed);
    push;
    term;
    full_check = None;
  }

let sum = commutative_monoid 0 (+)

let void c = {
  seed = ();
  push = (fun _ () -> ());
  term = (fun () -> c);
  full_check = Some (fun () -> true);
}
