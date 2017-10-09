let id x = x
let (>>) f g x = g (f x)
let ($$) f x = f x
let swap f a b = f b a
let swap_pair (a,b) = (b,a)

module Option = struct
  let map f = function
    | None -> None
    | Some x -> Some (f x)

  let when_defined ox f = map f ox
end

