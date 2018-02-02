type 'a t = { fold: 'b 'c. ('a,'b,'c) Action.t -> 'b -> 'c }

open Action

let fold xs = xs.fold
let fold_sync xs push = xs.fold (sync_action push)
let fold_lwt xs push = xs.fold (lwt_action push)

let empty =
  let fold red = red.term in
  { fold }

let single x =
  let fold red = red.push x in
  { fold }

let range min max =
  let fold red =
    let rec loop i acc =
      if i > max
      then red.term acc
      else red.cont (red.push i acc) (loop (i+1))
    in loop min
  in
  { fold }

let of_list xs = 
  let fold red =
    let (>>=) = red.cont in
    let rec loop xs acc = match xs with
      | [] -> red.term acc
      | x::xs -> red.push x acc >>= loop xs
    in
    loop xs
  in
  { fold }

let map f xs =
  let fold red = xs.fold (Action.map f red) in
  { fold }

let filter p xs =
  let fold red = xs.fold (Action.filter p red) in
  { fold }

let iter red xs = xs.fold red

let flat_map f xs =
  let fold red = xs.fold (Action.flat_map iter f red) in
  { fold }

