type some

type none

type ('a,'b) t =
  | Cap : 'a -> ('a, some) t
  | NoCap : ('a, none) t

let some a = Cap a

let none = NoCap

let get = function
  | Cap a -> a

let map (type a) (type b) (type c): (a -> b) -> (a,c) t -> (b,c) t
  = fun f -> function
  | Cap a -> Cap (f a)
  | NoCap -> NoCap
