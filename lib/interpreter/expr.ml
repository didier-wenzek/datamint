(* An expression
   - Using the type 'var to denote a variable
     (in practice, either a name or de bruijn indice).
   - Using the type 'def to introduce a variable
     (in practice, either a name or a (type, name) pair
     or even just a type when using de bruijn indices).
*)
type ('var,'def) expr =
  | Var of 'var
  | Lit of 'def * string         (* The definition 'def is used to hold the literal type *)
  | Fun of 'def * ('var,'def) expr
  | App of ('var,'def) expr * ('var,'def) expr
  | Unit
  | Pair of ('var,'def) expr * ('var,'def) expr
  | Fst of ('var,'def) expr
  | Snd of ('var,'def) expr
  | EmptyRecord
  | Record of ('var,'def) expr * string * ('var,'def) expr (* t with { f = expr } *)
  | Dot of 'def * string         (* The definition 'def is only used to hold the target record type.
                                    Hence, the definition name is useless.
                                    An expression `expr.f is transformed into `App(Dot("_",f),expr)`. *)
  | Tag of 'def * string * ('var,'def) expr (* Here, the definition 'def is used to carry the target case type *)
  | EmptyCase
  | Case of string * 'def * ('var,'def) expr * ('var,'def) expr  (* case { A(x) -> B } otherwise c *)
  | Switch of ('var,'def) expr * ('var,'def) expr                (* case [e] of [e] *)
  | Shape of Datatype.t
  | Generic of ('var,'def) expr    (* wrap an expression of type (Shape(a) -> a -> b) => the compiler while provide the shape *)

type name = string
type untyped_expr = (name, name) expr
type typed_expr = (name * (Type.typ list), name * Type.typ) expr
type de_bruijn_expr = (DeBruijn.t, unit) expr
type typed_de_bruijn_expr = (DeBruijn.t, Type.typ) expr

exception Unknown_value of string

(* Show *)
let id x = x
let p = Format.sprintf
let show_expr show_var show_def =
  let rec show = function
  | Var x -> p "Var(%s)" (show_var x)
  | Unit -> "()"
  | Lit (_,l) -> p "%s" l
  | Fun (x, body) -> p "Fun(%s -> %s)" (show_def x) (show body)
  | App (f, x) -> p "App(%s,%s)" (show f) (show x)
  | Pair (f, x) -> p "Pair(%s,%s)" (show f) (show x)
  | Fst x -> p "Fst(%s)" (show x)
  | Snd x -> p "Snd(%s)" (show x)
  | EmptyRecord -> p "{}"
  | Record (r,f,e) -> p "Record(%s,%s,%s)" (show r) f (show e)
  | Dot (r,f) -> p "Dot(%s,%s)" (show_def r) f
  | Tag (c,t,e) -> p "Tag(%s,%s,%s)" (show_def c) t (show e)
  | EmptyCase -> p "case {}"
  | Case (c,x,body,otherwise) -> p "Case(%s,%s,%s,%s)" c (show_def x) (show body) (show otherwise)
  | Switch (expr,cases) -> p "Switch(%s,%s)" (show expr) (show cases)
  | Shape s -> p "Shape(%s)" (Datatype.show s)
  | Generic f -> p "Generic(%s)" (show f)
  in show

let show_untyped = show_expr id id
let show_de_bruijn = show_expr DeBruijn.show (fun () -> "")

let show_typed_var show_var show_type (x,ts) =
  List.fold_left (fun s t -> p "%s[%s]" s (show_type t)) (show_var x) ts

let show_typed_def show_type (x,t) =
  p "%s:%s" x (show_type t)

let show_typed =
  show_expr (show_typed_var id Type.show) (show_typed_def Type.show)

let show_de_bruijn_typed =
  show_expr DeBruijn.show Type.show

let show_env_types env = 
  List.fold_left (fun env value -> p "%s %s |" env (show_typed_var id Type.show value)) "|" env

(* Transform an untyped expression
   into an expression where each variable is introduced with its type.

   `Lam(x, body)` is transformed into `Lam(x:t, body)`
   where t is a mono-morphic type variable.

   `Var(x)` is transformed into `Var(x: [a, b, ...]`
   where a mono-morphic type variable is introduced
   for each generic variable of the polymorphic type of x.

   Return the transformed expression along its type.
*)
let rec infer_types env = function
  | Var x -> Type.(
    match Env.lookup_var x env with
    | Some polytype_eqn ->
      let vars_eqn = polytype_eqn.vars_eqn in
      let type_eqn = polytype_eqn.type_eqn in
      (Var (x, vars_eqn), type_eqn)
    | _ -> raise (Unknown_value x)
  )

  | Unit -> (Unit, Type.UnitType)
  | Lit (t_name,l) ->
    let t = Type.TypeSym t_name in
    (Lit ((t_name,t),l), t)

  | Fun (x,e) -> Type.(
    let x_t = if x = "()" then UnitType else new_typevar () in
    let f_env = Env.introduce_var x x_t env in
    let e, e_t = infer_types f_env e in
    (Fun((x,x_t),e), FunType(x_t,e_t))
  )

  | App (f,x) -> Type.(
    let f, f_t = infer_types env f in
    let x, x_t = infer_types env x in
    let r_t = new_typevar () in
    unify f_t (FunType (x_t,r_t));
    (App (f,x), r_t)
  )

  | Pair (a,b) -> Type.(
    let a, a_t = infer_types env a in
    let b, b_t = infer_types env b in
    (Pair (a,b), PairType (a_t,b_t))
  )

  | Fst p -> Type.(
    let p, p_t = infer_types env p in
    let a_t = new_typevar () in
    let b_t = new_typevar () in
    unify p_t (PairType (a_t,b_t));
    (Fst p, a_t)
  )

  | Snd p -> Type.(
    let p, p_t = infer_types env p in
    let a_t = new_typevar () in
    let b_t = new_typevar () in
    unify p_t (PairType (a_t,b_t));
    (Snd p, b_t)
  )

  | EmptyRecord -> Type.(
    (EmptyRecord, EmptyRecordType)
  )

  | Record (r,p,p_e) -> Type.(
    let r, r_t = infer_types env r in
    let p_e, p_t = infer_types env p_e in
    unify r_t (record_requirement ());
    (Record (r,p,p_e), RecordType(r_t,p,p_t))
  )

  | Dot (r,p) -> Type.(
    let p_t = new_typevar () in
    let r_t = record_field_requirement p p_t in
    (Dot ((r,r_t),p), FunType(r_t,p_t))
  )

  | Tag (options,tag,e) -> Type.(
    let e, e_t = infer_types env e in
    let res_t = new_typevar () in
    let this_case_t = FunType (e_t, res_t) in
    let options_t = record_field_requirement tag this_case_t in
    (Tag ((options,options_t),tag,e), SumType(options_t, res_t))
  )

  | Case (tag,x,body,otherwise) -> Type.(
    let x_t = if x = "()" then UnitType else new_typevar () in
    let body_env = Env.introduce_var x x_t env in
    let body, res_t = infer_types body_env body in
    let otherwise, otherwise_t = infer_types env otherwise in
    let this_case_t = FunType (x_t,res_t) in
    let other_cases_t = new_typevar () in
    let all_cases_t = RecordType(other_cases_t, tag, this_case_t) in
    unify other_cases_t (record_requirement ());
    unify otherwise_t (CaseType (other_cases_t, res_t));
    (Case (tag,(x,x_t),body,otherwise), CaseType(all_cases_t, res_t))
  )

  | EmptyCase -> Type.(
    let r_t = new_typevar () in
    (EmptyCase, CaseType(EmptyRecordType, r_t))
  )

  | Switch (expr, cases) -> Type.(
    let expr, expr_t = infer_types env expr in
    let cases, cases_t = infer_types env cases in
    let options_t = new_typevar () in
    let res_t = new_typevar () in
    unify expr_t (SumType (options_t, res_t));
    unify cases_t (CaseType (options_t, res_t));
    (Switch (expr, cases), res_t)
  )

  | Shape s -> Type.(
    let t = shape_type s in
    (Shape s, ShapeType t)
  )

  | Generic f -> Type.(
    let f, f_t = infer_types env f in
    let g_t = new_typevar () in
    let a_t = new_typevar () in
    let b_t = new_typevar () in
    unify f_t (FunType (ShapeType a_t, g_t));
    unify g_t (FunType (a_t,b_t));
    (Generic f, g_t)
  )

and unify_type env x_t e =
  let e, e_t = infer_types env e in
  Type.unify e_t x_t;
  e

and shape_type = function
  | Datatype.Atomic n -> Type.TypeSym n
  | Datatype.Tuple [] -> Type.UnitType
  | Datatype.Tuple [s] -> shape_type s
  | Datatype.Tuple (s::ss) ->
    Type.PairType(shape_type s, shape_type (Datatype.Tuple ss))
  | Datatype.Record fields ->
    List.fold_left (fun r (f,s) -> Type.RecordType (r, f, shape_type s)) Type.EmptyRecordType fields
  | Datatype.List s -> Type.GeneratorType (shape_type s)

(* Rewrite a typed expression,
   replacing all the bound type variables with their type value
   and naming all unbound type variables using [genvars].
*)
let rec remove_typevars genvars = function
  | Var (x, eqn_args) ->
    let type_args = List.map genvars eqn_args in
    Var (x, type_args)

  | Unit -> Unit
  | Lit ((t_name,t_eqn),l) ->
    let t = genvars t_eqn in
    Lit ((t_name,t),l)

  | Fun ((x,eqn_t),e) ->
    let x_t = genvars eqn_t in
    let e = remove_typevars genvars e in
    Fun ((x,x_t),e)

  | App (f,x) ->
    let f = remove_typevars genvars f in
    let x = remove_typevars genvars x in
    App (f,x)

  | Pair (a,b) ->
    let a = remove_typevars genvars a in
    let b = remove_typevars genvars b in
    Pair (a,b)

  | Fst a ->
    let a = remove_typevars genvars a in
    Fst a

  | Snd a ->
    let a = remove_typevars genvars a in
    Snd a

  | EmptyRecord -> EmptyRecord

  | Record (r,p,e) ->
    let r = remove_typevars genvars r in
    let e = remove_typevars genvars e in
    Record (r,p,e)

  | Dot ((r,r_eqn),p) ->
    let r_t = genvars r_eqn in
    Dot ((r,r_t),p)

  | Tag ((c,c_eqn),t,e) ->
    let c_t = genvars c_eqn in
    let e = remove_typevars genvars e in
    Tag ((c,c_t),t,e)

  | EmptyCase -> EmptyCase

  | Case (t,(x,eqn_t),body,otherwise) ->
    let x_t = genvars eqn_t in
    let body = remove_typevars genvars body in
    let otherwise = remove_typevars genvars otherwise in
    Case (t,(x,x_t),body,otherwise)

  | Switch (e,c) ->
    let e = remove_typevars genvars e in
    let c = remove_typevars genvars c in
    Switch (e,c)

  | Shape s ->
    Shape s

  | Generic f ->
    let f = remove_typevars genvars f in
    Generic f

(* Finalize type inference,
   removing the type equations
   and introducing the free type variables.
*)
let infer_types env expr =
  let eqn_typed_expr, type_eqn = infer_types env expr in
  let polytype, genvars = Type.mapping_eqn_to_polytype type_eqn in
  let typed_expr = remove_typevars (Type.partial_eqn_to_type genvars) eqn_typed_expr in
  (typed_expr, polytype)

let typeof env e =
  let (_,t) = infer_types env e in t

let typed_expr env e =
  let (e,_) = infer_types env e in e

(* Extract the set of free variables of an expression. *)
let free_vars var_name def_name =
  let rec loop vars free_vars = function
    | Var x -> (
      let name = var_name x in
      if List.mem name vars
      || List.mem name free_vars
      then free_vars
      else name::free_vars
    )

    | Unit | Lit (_,_) | EmptyRecord | EmptyCase -> free_vars

    | Fun (x, body) ->
      let name = def_name x in
      loop (name::vars) free_vars body

    | Case (t,x,body,otherwise) ->
      let name = def_name x in
      loop vars (loop (name::vars) free_vars body) otherwise

    | App (a, b)
    | Pair (a, b)
    | Record (a,_,b)
    | Switch (a, b) ->
      loop vars (loop vars free_vars a) b

    | Fst e
    | Snd e
    | Tag (_,_,e)
    | Generic e  ->
      loop vars free_vars e

    | Dot (_,_)
    | Shape _ ->
      free_vars
  in
  fun expr -> loop [] [] expr

(* Rewrite an expression replacing all the variable definitions and usages.

   using an environment of variables (augmented each time a new variable is introduced)
   and two rewrite functions which give a new variable name in a given context.

*)
let rewrite_variables:
      (('a,'b) expr -> 'c) -> ('b -> 'c -> 'c) -> ('a -> 'c -> 'd) -> ('b -> 'c -> 'e) -> ('a,'b) expr -> ('d,'e) expr
 = fun init_env                augment_env         rewrite_var         rewrite_def ->
  let rec loop env = function
    | Var x -> Var (rewrite_var x env)

    | Unit -> Unit

    | Lit (t,l) ->
      Lit (rewrite_def t env,l)

    | Fun (x, body) ->
      let body_env = augment_env x env in
      Fun (rewrite_def x env, loop body_env body)

    | App (f, x) ->
      App (loop env f, loop env x)

    | Pair (a, b) ->
      Pair (loop env a, loop env b)

    | Fst p ->
      Fst (loop env p)

    | Snd p ->
      Snd (loop env p)
      
    | EmptyRecord -> EmptyRecord

    | Record (r,p,e) ->
      Record (loop env r, p, loop env e)

    | Dot (r,p) ->
      Dot (rewrite_def r env,p)

    | Tag (c,t,e) ->
      Tag (rewrite_def c env, t, loop env e)

    | EmptyCase -> EmptyCase

    | Case (t,x,body,otherwise) ->
      let body_env = augment_env x env in
      Case (t, rewrite_def x env, loop body_env body, loop env otherwise)

    | Switch (e,c) ->
      Switch (loop env e, loop env c)

    | Shape s ->
      Shape s

    | Generic f ->
      Generic (loop env f)
  in
  fun expr -> 
    loop (init_env expr) expr

(* Transform an expression where the variables are denoted by name
   into an expression where variables are denoted using De Bruijn indices.

   The transformed expression is returned along the list of free variables.
*)
let de_bruijn var_name def_name idx_name idx_def =
  let augment_env x vars = (def_name x)::vars in
  let rewrite_var = idx_name in
  let rewrite_def = idx_def in
  fun expr -> 
    let free_vars = free_vars var_name def_name expr in
    let init_env e = free_vars in
    let rewrite = rewrite_variables init_env augment_env rewrite_var rewrite_def in
    (rewrite expr, free_vars)

let de_bruijn_expr =
  let no_def _ _ = () in
  de_bruijn id id DeBruijn.idx_of no_def

let de_bruijn_typed =
  let var_name name_type_args = name_type_args in
  let var_idx name_type_args vars = DeBruijn.idx_of name_type_args vars in
  let def_name (name, _) = (name,[]) in  (* A lambda introduces a mono-morphic variable *)
  let def_idx (_, type_expr) vars = type_expr in
  de_bruijn var_name def_name var_idx def_idx

let rename_variables rewrite_var =
  let init_env _ = () in
  let augment_env x () = () in
  let rewrite_var x () = rewrite_var x in
  let rewrite_def x () = x in
  rewrite_variables init_env augment_env rewrite_var rewrite_def
