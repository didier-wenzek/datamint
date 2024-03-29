let id x = x
let (>>) f g x = g (f x)
let constant c _ = c

(* Function that lets you return early from a computation.
   Adapted from Alan Frish's version of https://ocaml.janestreet.com/?q=node/91,
   with the constraint that the return function is only used
   in contexts having the same type as the whole expression.

   let sum_until_first_negative list =
     with_return (fun return ->
       List.fold_left (fun acc x -> if x >= 0 then acc + x else return acc)
                      0
                      list
     )
*)
let with_return (type t) f =
  let module M = struct exception Return of t end in
  try f (fun x -> raise (M.Return x)) with M.Return x -> x

let with_resource init term config f =
  let x = init config in
  try
    let y = f x in
    let () = term x in
    y
  with e ->
    let () = term x in
    raise e

module Option = struct

  let none = None
  let some x = Some x

  let map f = function
    | None -> None
    | Some x -> Some (f x)

  let flat_map f = function
    | None -> None
    | Some x -> f x

  let (>>=) x f = match x with
    | None -> None
    | Some x -> f x

  let (>|=) x f = match x with
    | None -> None
    | Some x -> Some (f x)                                                                                                                                                                                 

  let default e = function
    | Some x -> x
    | None -> e

  let if_none f = function
    | Some x -> x
    | None -> f ()

  let none_if_notfound f x =
    try Some (f x)
    with Not_found -> None

  let combine op x y = match x,y with
    | Some x , Some y -> Some (op x y)
    | _ -> None

  let of_result = function
    | Ok x -> Some x
    | _ -> None
end

module Result = struct

  let ok s = Ok s
  let error e = Error e

  let map f = function
    | Ok x -> Ok (f x)
    | Error _ as e -> e  (* If the case is coded as `| e -> e`,
                            Then the compiler wrongly infers the type of `map` as `('a -> 'a) -> 'a t -> 'a t` *)

  let flat_map f = function
    | Ok x -> f x
    | Error _ as e -> e

  let (>>=) r f = match r with
    | Ok x -> f x
    | Error _ as e -> e

  let (>|=) r f = match r with
    | Ok x -> Ok (f x)
    | Error _ as e -> e

  let on_error f = function
    | Ok x -> x
    | Error e -> f e

  let try_apply f g x =
    try Ok (f x)
    with e -> Error (g e)

  let iter_apply f =
    let rec loop rs = function
      | [] -> Ok (List.rev rs)
      | x::xs -> (
        match f x with
        | Ok r -> loop (r::rs) xs
        | Error r -> Error r
      )
    in
    loop []
end

module Either = struct
  type ('a,'b) t = Left of 'a | Right of 'b
end

module Time = struct

  let time_ms f x =
    let t0 = Unix.gettimeofday () in                                                                                                                                                                         
    let r = f x in
    let t1 = Unix.gettimeofday () in
    let d = (t1 -. t0) *. 1000.0 in
    (int_of_float d,r)    

end
