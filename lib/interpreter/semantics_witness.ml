module type S = sig
  include Semantics.S

  type 'a witness = 'a repr Type_witness.t
  type 'e env_witness
  type _ dyn_cexpr = DynCExpr : 'a witness * ('a,'e) cexpr -> 'e dyn_cexpr

  val empty_env_sig: empty_env env_witness
  val extent_env_sig: 'a witness -> 'e env_witness -> ('a, 'e) exenv env_witness

  val lookup: 'e env_witness -> DeBruijn.t -> 'e dyn_cexpr

  val show_env_sig: 'e env_witness -> string
end

module Make(Sem: S with type 'a repr = 'a) = struct (* FIXME : remove the type constraint *)
  include Sem

  type dyn_env = DynEnv : 'e Sem.env_witness * 'e Sem.env -> dyn_env

  let empty_dyn_env = DynEnv(Sem.empty_env_sig, Sem.empty_env)

  let extend_dyn_env dyn_value dyn_env =
    let Type_witness.Dyn(expr_sig, expr) = dyn_value in
    let DynEnv(env_sig, env) = dyn_env in
    DynEnv(Sem.extent_env_sig expr_sig env_sig, Sem.extent_env expr env)

  let make_dyn_env dyn_values =
    (* Warning ! the environment must be extended from right to left *)
    List.fold_right extend_dyn_env dyn_values empty_dyn_env
end

module TaglessFinal: S
  with type 'a repr = 'a
  and type ('a,'b) exenv = 'a * 'b
  and type ('a,'b) cexpr = ('b -> 'a)
= struct
  include Semantics.TaglessFinal

  type 'a witness = 'a repr Type_witness.t

  type _ env_witness = 
    | EmptyEnvSig: empty_env env_witness
    | ExtendedEnvSig: 'a witness * 'e env_witness -> ('a, 'e) exenv env_witness

  type _ dyn_cexpr = DynCExpr : 'a witness * ('a,'e) cexpr -> 'e dyn_cexpr

  let empty_env_sig = EmptyEnvSig
  let extent_env_sig var_sig env_sig = ExtendedEnvSig(var_sig, env_sig)

  let rec lookup: type e. e env_witness -> DeBruijn.t -> e dyn_cexpr
    = fun env_sig idx -> match env_sig, DeBruijn.index idx with
    | ExtendedEnvSig(x_sig, _), DeBruijn.VZ ->
      DynCExpr(x_sig, varZ)

    | ExtendedEnvSig(_, inner_env_sig), DeBruijn.VS v ->
      let DynCExpr(x_sig, x_cexpr) = lookup inner_env_sig v in
      DynCExpr(x_sig, varS x_cexpr)
    
    | EmptyEnvSig, _ ->
      raise (Semantics.Compilation_error "Compilation of an open term")

  let rec show_env_sig: type e. e env_witness -> string = function
    | EmptyEnvSig -> "|"
    | ExtendedEnvSig(x,e) -> Format.sprintf "|%s%s" (Type_witness.show x) (show_env_sig e)

end
