type 'a t = {
  missing_items: int;
  asc_items: 'a list;
}

let items xs =
  Bounded.of_list (List.rev xs.asc_items)

let empty n = {
  missing_items = n;
  asc_items = [];
}

let single n x = {
  missing_items = n - 1;
  asc_items = [x];
}

let add k v =
  let same x y = (k y) = (k x) in
  let gt x y = (v x) > (v y) in
  let insert x =
    let rec loop is_new lowers = function
      | y :: ys when gt x y ->
        if same x y
        then loop false lowers ys
        else loop is_new (y::lowers) ys
      | ys ->
        if List.exists (same x) ys
        then (false, List.rev_append lowers ys)
        else (is_new, List.rev_append lowers (x::ys))
    in
    loop true []
  in
  let replace x = function
    | y :: ys when gt x y ->
      let (is_new,ys) = insert x ys in
      if is_new then ys else y :: ys
    | ys -> ys
  in
  fun n x xs ->
    if xs.missing_items > 0
    then 
      let (is_new, asc_items) = insert x xs.asc_items in
      let delta = if is_new then 1 else 0 in
      { missing_items = xs.missing_items - delta;
        asc_items; }
    else
      { xs with
        asc_items = replace x xs.asc_items; }

let reducer k v n =
  let open Reducer in
  {
    seed = empty n;
    push = add k v n;
    term = items;
    full_check = if n > 0 then None else Some (fun _ -> true);
  }
