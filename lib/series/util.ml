let id x = x
let (>>) f g x = g (f x)
let constant c x = c

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
end

module Result = struct

  let on_error f = function
    | Ok x -> x
    | Error e -> f e

end
