type t = int
type index = VZ | VS of t

let zero = 0
let succ = succ
let index idx = if idx = 0 then VZ else VS (pred idx)
let of_int idx = idx

let idx_of x =
  let rec loop idx = function
    | [] -> raise Not_found
    | y::_ when x = y -> idx
    | _::vars -> loop (succ idx) vars
  in
  loop zero

let rec find idx xs = match idx, xs with
  | 0, x::_ -> x
  | idx, _::xs -> find (pred idx) xs
  | _, [] -> raise Not_found

let show = string_of_int
let pp = Format.pp_print_int

let name n =
  if n < 26
  then Format.sprintf "%c" (char_of_int ((int_of_char 'a') + n))
  else Format.sprintf "z%d" (n - 26)

let names xs =
  let _,names =
    List.fold_left (fun (n,names) x -> (n+1, (name n)::names)) (0,[]) xs
  in
  List.rev names

let compare a b = a - b

let find_eq equal values value =
  let rec loop idx = function
    | [] -> raise Not_found
    | x::_ when equal value x -> idx
    | _::xs -> loop (succ idx) xs
  in
  loop zero values
