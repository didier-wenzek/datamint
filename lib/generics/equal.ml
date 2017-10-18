type 'a equal = 'a -> 'a -> bool

module EqIntr : Interpretation.S
  with type 'a tc = 'a equal
= struct
  type 'a tc = 'a equal

  (* Primitive types *)

  let unit () () = true
  let int x y = x = y
  let int64 = Int64.equal
  let float x y = x = y
  let bool x y = x = y
  let string = String.equal

  (* Tuples *)

  let pair head_eq tail_eq (h1, t1) (h2, t2) =
    (head_eq h1 h2) && (tail_eq t1 t2)

  let tuple fields_eq =
    fields_eq

  (* Records *)

  let empty_record = unit
  let field name field_eq = field_eq
  let (&) = pair
  let record = tuple

  (* Lists *)

  let list = Series.Bounded.equal

  (* Maps *)

  let mapping = Series.Mapping.equal
end

let generic: 'a Repr.t -> 'a equal =
  let open Repr in
    fun (type a) repr -> 
      let module R = (val repr : Repr.S with type a = a) in 
      let module N = R.Interpret (EqIntr)
      in N.result
