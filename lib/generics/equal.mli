type 'a equal = 'a -> 'a -> bool

val generic: 'a Repr.t -> 'a equal
