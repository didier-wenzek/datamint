exception Already_bound

module type Expr = sig
  type 'var u_expr

  val to_var: 'var u_expr -> 'var option
end

module VarExpr(E: Expr): sig
  type var
  type expr = var E.u_expr

  val new_var: unit -> var

  val unify: var -> var -> unit
  val equal: var -> var -> bool

  val set_value: var -> expr -> unit
  val merge: (expr -> expr -> expr) -> (var -> expr -> bool) -> var -> var -> unit
  val update: (expr -> expr -> expr) -> (var -> expr -> bool) -> var -> expr -> unit

  val assign: var -> expr -> unit
  val is_bound: var -> bool
  val value: var -> expr option
end
