module type S = sig
  type env
  type value = Type_witness.poly_dyn
  type expr = Expr.untyped_expr
  type typ = Type.polytyp

  val initial_env: env
  val define: string -> typ * value -> env -> env

  val compile: env -> expr -> typ * value
  val declare: string -> expr -> env -> env

  val show_typed_value: typ -> value -> string*string

  val typeof: env -> expr -> typ
end

module type With_bool_support = sig
  type bool_repr

  val bool_t: Type.typ
  val bool_sig: bool_repr Type_witness.t

  val bool_of_repr: bool_repr -> bool
  val repr_of_bool: bool -> bool_repr

  val adapt_predicate: ('a -> bool_repr) -> ('a -> bool)
end

module type With_int_support = sig
  type int_repr

  val int_t: Type.typ
  val int_sig: int_repr Type_witness.t

  val int_of_repr: int_repr -> int
  val repr_of_int: int -> int_repr
end

module type With_string_support = sig
  type string_repr

  val string_t: Type.typ
  val string_sig: string_repr Type_witness.t

  val string_of_repr: string_repr -> string
  val repr_of_string: string -> string_repr
end

module Make(Sem: Semantics_witness.S with type 'a repr = 'a): S
