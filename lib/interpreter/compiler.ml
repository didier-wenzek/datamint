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

module Make(S: Semantics_witness.S with type 'a repr = 'a) = struct

  module Sem = Semantics_witness.Make(S)

  type type_sig = Type_witness.type_sig
  type value = Type_witness.poly_dyn
  type expr = Expr.untyped_expr
  type typ = Type.polytyp

  let rec compile : type e. type_sig list -> e Sem.env_witness -> Expr.typed_de_bruijn_expr -> e Sem.dyn_cexpr
    = fun gen_types env expr ->
(*  Format.printf "XOXOX: compile %s in %s\n%!" (Expr.show_de_bruijn_typed expr) (Sem.show_env_sig env); *)
    match expr with
    | Expr.Lit (lit_t,lit_string) ->
      let Type_witness.Dyn(lit_sig, lit_val) = Type_witness.dyn_of_literal lit_t lit_string in
      Sem.DynCExpr(lit_sig, Sem.constant lit_val)

    | Expr.Unit
    | Expr.EmptyRecord ->
      Sem.DynCExpr(Type_witness.unit, Sem.unit)

    | Expr.Var v -> Sem.lookup env v

    | Expr.Fun (x_t, body) -> (
      let Type_witness.Sig x_sig = Type_witness.eval_type gen_types x_t in
      let extended_env = Sem.extent_env_sig x_sig env in
      let Sem.DynCExpr(body_sig, body_cexpr) = compile gen_types extended_env body in
      Sem.DynCExpr(Type_witness.lam x_sig body_sig, Sem.lam body_cexpr)
    )

    | Expr.App (f,x) -> (
      match compile gen_types env f with
      | Sem.DynCExpr(Type_witness.FunSig(a_t, r_t), f_cexpr) -> (
        let Sem.DynCExpr(x_t, x_cexpr) = compile gen_types env x in
        match Type_witness.eq_type x_t a_t with
        | Some Type_witness.Eq ->
          Sem.DynCExpr(r_t, Sem.app f_cexpr x_cexpr)
        | _ ->
          (* Printf.printf "XOXOX: expect '%s' but got '%s'\n%!" (Type_witness.show x_t) (Type_witness.show a_t); *)
          assert false 
      )
      | Sem.DynCExpr(_huh,_) ->
          (* Printf.printf "XOXOX: expect fun but got '%s'\n%!" (Type_witness.show huh); *)
          assert false
    )

    | Expr.Pair (a,b) -> (
      let Sem.DynCExpr(a_sig, a_cexpr) = compile gen_types env a in
      let Sem.DynCExpr(b_sig, b_cexpr) = compile gen_types env b in
      Sem.DynCExpr(Type_witness.pair a_sig b_sig, Sem.pair a_cexpr b_cexpr)
    )

    | Expr.Fst p -> (
      match compile gen_types env p with
      | Sem.DynCExpr(Type_witness.PairSig(a_sig, _), p_cexpr) ->
        Sem.DynCExpr(a_sig, Sem.fst p_cexpr)
      | _ -> assert false 
    )

    | Expr.Snd p -> (
      match compile gen_types env p with
      | Sem.DynCExpr(Type_witness.PairSig(_, b_sig), p_cexpr) ->
        Sem.DynCExpr(b_sig, Sem.snd p_cexpr)
      | _ -> assert false 
    )

    | Expr.Record (a,p,b) ->
      let Sem.DynCExpr(a_sig, a_cexpr) = compile gen_types env a in
      let Sem.DynCExpr(b_sig, b_cexpr) = compile gen_types env b in
      Sem.DynCExpr(Type_witness.record a_sig p b_sig, Sem.pair b_cexpr a_cexpr)

    | Expr.Dot (r_t, p) -> 
      let Type_witness.Sig r_sig = Type_witness.eval_type gen_types r_t in
      let Type_witness.FieldGetter(get,f_sig) = Type_witness.field_getter p r_sig in
      Sem.DynCExpr(Type_witness.lam r_sig f_sig, Sem.constant get)

    | Expr.Tag (c_t, tag, e) ->
      let e = Expr.rename_variables DeBruijn.succ e in
      compile gen_types env Expr.(Fun (c_t, App(App(Dot (c_t,tag), Var DeBruijn.zero), e)))

    | Expr.EmptyCase
    | Expr.Case (_, _, _, _) ->
      compile gen_types env (cases_record expr)

    | Expr.Switch (expr,cases) ->
      compile gen_types env Expr.(App (expr, cases))

    | Expr.Shape s ->
      let Type_witness.Dyn(shape_sig, shape_val) = Type_witness.dyn_of_shape s in
      Sem.DynCExpr(shape_sig, Sem.constant shape_val)

    | Expr.Generic f -> (
      match compile gen_types env f with
      | Sem.DynCExpr(Type_witness.FunSig(Type_witness.ShapeSig x_t, Type_witness.FunSig(a_t,b_t)), gen_cexpr) -> (
        match Type_witness.eq_type x_t a_t with
        | Some Type_witness.Eq -> (
          match Type_witness.gen_val_of_sig x_t with
          | Some x_gen_repr ->
            let f_cexpr = Sem.app gen_cexpr (Sem.constant x_gen_repr) in
            Sem.DynCExpr(Type_witness.FunSig(a_t,b_t), f_cexpr)
          | None ->
            (* FIXME: must either throw a compilation error or prevent the error upstream. *)
            assert false 
        )
        | _ ->
          assert false
      )
      | Sem.DynCExpr(_huh,_) ->
        assert false (* generic can only be applied to a function with first argument is a shape.*)
    )

  and cases_record = Expr.(function
    | EmptyCase -> EmptyRecord
    | Case (tag, x_t, body, otherwise) ->
      let otherwise = cases_record otherwise in
      Record(otherwise, tag, Fun(x_t, body))
    | _ -> assert false
  )

  module Dict = Map.Make(String)

  type env = {
    types : Type.Env.env;
    values : value Dict.t
  }

  let empty_env = {
    types = Type.Env.empty;
    values = Dict.empty;
  }

  let lookup values name =
    try Dict.find name values
    with Not_found -> raise (Expr.Unknown_value name)

  let define name (expr_type, dyn_value) env = {
    types = Type.Env.set_typeof name expr_type env.types;
    values = Dict.add name dyn_value env.values;
  }

  let lookup_qualified values (name,type_args) =
    (lookup values name, type_args)

  let eval_polytype_expr free_typed_values de_bruijn_typed_expr gen_vars =
    let rec loop gen_types = function
      | [] ->
        let gen_types = List.rev gen_types in
        let free_typed_values = List.map (Type_witness.specialize_polydyn gen_types) free_typed_values in
        let Sem.DynEnv(free_sigs,free_vals) = Sem.make_dyn_env free_typed_values in
        let Sem.DynCExpr (expr_sig, cexpr) = compile gen_types free_sigs de_bruijn_typed_expr in
        Type_witness.MonoDyn(expr_sig, Sem.eval cexpr free_vals)

      | false :: gen_typevars ->
        let gen_types = Type_witness.(Sig VoidSig)::gen_types in
        loop gen_types gen_typevars

      | true :: gen_typevars ->
        Type_witness.PolyDyn { make = fun gen_type ->
          let gen_types = Type_witness.Sig(gen_type)::gen_types in
          loop gen_types gen_typevars
        }
    in loop [] (List.rev gen_vars)
    
  let compile env expr =
    let typed_expr, expr_type = Expr.infer_types env.types expr in
    let de_bruijn_typed_expr, free_vars = Expr.de_bruijn_typed typed_expr in
    let free_typed_values = List.map (lookup_qualified env.values) free_vars in
    let gen_vars = Type.gen_vars expr_type in
 (* Format.printf "XOXOX: eval %s of type %s in env %s\n%!" (Expr.show_de_bruijn_typed de_bruijn_typed_expr) (Type.show_polytype expr_type) (Expr.show_env_types free_vars); *)
    (expr_type, eval_polytype_expr free_typed_values de_bruijn_typed_expr gen_vars)

  let typeof env = Expr.typeof env.types

  let show_typed_value = Type_witness.show_poly_dyn

  let declare name expr env =
    define name (compile env expr) env

  let initial_env = empty_env
    |> declare "fst" Expr.(Fun ("x", Fst (Var "x")))
    |> declare "snd" Expr.(Fun ("x", Snd (Var "x")))
    |> declare "generic" Expr.(Fun ("f", Generic (Var "f")))
end
