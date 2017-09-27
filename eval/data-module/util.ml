let id x = x
let (>>) f g x = g (f x)
let ($$) f x = f x

module Option = struct
  let map f = function
    | None -> None
    | Some x -> Some (f x)

  let when_defined ox f = map f ox
end

