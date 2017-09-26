module FieldSet : Map.S with type key = string

type 'var type_expr =
  | TypeVar of 'var
  | TypeSym of string
  | FunType of 'var type_expr * 'var type_expr
  | UnitType
  | PairType of 'var type_expr * 'var type_expr
  | EmptyRecordType
  | RecordType of 'var type_expr * string * 'var type_expr (* r with f = e *)
  | CaseType of 'var type_expr * 'var type_expr (* CaseType(options,t) groups a set of functions with a shared returned type t. *)
  | SumType of 'var type_expr * 'var type_expr  (* SumType(options,t) behaves a as function taking a CaseType(options,t)
                                                   and picking one of the options to forge some t value. *)
  | GeneratorType of 'var type_expr
  | ReducerType of 'var type_expr * 'var type_expr * 'var type_expr
  | ShapeType of 'var type_expr

type 'var type_constraint =
  | NoConstraint
  | RequiredFields of 'var type_expr FieldSet.t     (* record with { ... } *)

type typ = DeBruijn.t type_expr
type polytyp = {
  vars_expr: DeBruijn.t type_constraint list;
  type_expr: DeBruijn.t type_expr;
}

type var
type eqn = var type_expr
type polyeqn = {
  vars_eqn: eqn list;
  type_eqn: eqn;
}

exception Unknown_type of string
exception Type_error of typ * typ
exception Missing_fields of typ

val show: typ -> string
val show_polytype: polytyp -> string
val show_polytype_type_expr: polytyp -> string
val show_polytype_constraints: polytyp -> string

val new_typevar: unit -> eqn

val record_requirement: unit -> eqn
val record_field_requirement: string -> eqn -> eqn
val record_fields: 'var type_expr -> 'var type_expr option * (string * 'var type_expr) list

val unify: eqn -> eqn -> unit
val unified: eqn -> eqn -> eqn

val eqn_to_polytype: eqn -> polytyp
val eqn_of_polytype: polytyp -> polyeqn

type var_mapping
val mapping_eqn_to_polytype: eqn -> polytyp * var_mapping
val partial_eqn_to_type: var_mapping -> eqn -> typ

val equal: polytyp -> polytyp -> bool

val is_abstract: polytyp -> bool
val gen_vars: polytyp -> bool list

module Env: sig
  type env

  val empty: env

  val set_typeof: string -> polytyp -> env -> env
  val typeof: string -> env -> polytyp option

  val introduce_var: string -> eqn -> env -> env
  val lookup_var: string -> env -> polyeqn option
end

val pair_t: typ -> typ -> typ
val fun_t: typ -> typ -> typ
val gen_t: int -> typ
val poly_t: int -> typ -> polytyp
val record_t: (string * typ) list -> typ
