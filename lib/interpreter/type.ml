module FieldSet = Map.Make(String)
module Dict = Dict.Make(String)

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
  | MappingType of 'var type_expr * 'var type_expr
  | ShapeType of 'var type_expr

type 'var type_constraint =
  | NoConstraint
  | RequiredFields of 'var type_expr FieldSet.t     (* record with { ... } *)

type 'var constrained_type_expr =
  | Type of 'var type_expr
  | Constraint of 'var type_constraint

module Var = Unification.VarExpr(struct
  type 'var u_expr = 'var constrained_type_expr

  let to_var = function
    | Type (TypeVar x) -> Some x
    | _ -> None
end)

type typ = DeBruijn.t type_expr
type polytyp = {
  vars_expr: DeBruijn.t type_constraint list;
  type_expr: DeBruijn.t type_expr;
}

type var = Var.var
type eqn = var type_expr
type polyeqn = {
  vars_eqn: eqn list;
  type_eqn: eqn;
}

exception Unknown_type of string
exception Type_error of typ * typ
exception Missing_fields of typ

let new_typevar () = TypeVar (Var.new_var ())

let eqn_of_constraint x = function
  | NoConstraint -> TypeVar x
  | c ->
    Var.set_value x (Constraint c);
    TypeVar x

let new_eqn_of_constraint c =
  eqn_of_constraint (Var.new_var ()) c

let record_requirement () =
  new_eqn_of_constraint (RequiredFields(FieldSet.empty))

let record_field_requirement p p_t = 
  new_eqn_of_constraint (RequiredFields(FieldSet.singleton p p_t))

let constraint_as_type = function
  | NoConstraint -> UnitType
  | RequiredFields fields -> 
      List.fold_left (fun r (f,t) -> RecordType(r,f,t)) EmptyRecordType (FieldSet.bindings fields)

(* Return the list of fields in the order they have been added. *)
let record_fields row =
  let rec loop fields = function
  | EmptyRecordType -> (None, fields)
  | RecordType(r,p,p_t) -> loop ((p,p_t)::fields) r
  | t -> (Some t, fields) 
  in loop [] row

let field_option (name, func) = match func with
  | FunType (t,_) -> (name, t)
  | _ -> assert false

let record_options constraints row =
  let rec loop fields = function
  | EmptyRecordType -> (None, fields)
  | RecordType(r,c_tag,c_t) -> loop ((c_tag,c_t)::fields) r
  | (TypeVar x) as t -> (
    match constraints x with
    | NoConstraint -> (Some t, fields)
    | RequiredFields required ->
      (Some t, List.rev_append (FieldSet.bindings required) fields)
  )
  | t -> (Some t, fields) 
  in
  (* the options are registred in their application order, but they are displayed in reverse order *)
  let otherwise, fields = loop [] row in
  let options = List.map field_option fields in
  (otherwise, List.rev options)

(* Returns the set of type variables used by a type expression. *)
let type_vars_expr push =
  let rec loop vars = function
    | TypeVar x -> push x vars
    | TypeSym _
    | UnitType
    | EmptyRecordType
    -> vars

    | GeneratorType t
    | ShapeType t
    -> loop vars t

    | FunType (t1,t2)
    | PairType (t1,t2)
    | RecordType (t1,_,t2)
    | CaseType (t1,t2)
    | SumType (t1,t2)
    | MappingType (t1,t2)
    -> loop (loop vars t1) t2

    | ReducerType (t1,t2,t3)
    -> loop (loop (loop vars t1) t2) t3

  in loop

let type_vars_constraint push vars =
  let push_field_vars name type_expr vars = type_vars_expr push vars type_expr in
  function
  | NoConstraint -> vars
  | RequiredFields fields ->
    FieldSet.fold push_field_vars fields vars

(* Show a type expression using the show_var function to display type variables,
   and the constraints function to get the constraints on a type variable. *)
let show_expr constraints show_var =
  let rec show = function
    | TypeVar x -> show_var x
    | TypeSym n -> n
    | FunType (FunType(_,_) as a,b) -> "("^(show a)^") -> "^(show b)
    | FunType (a,b) -> (show a)^" -> "^(show b)
    | UnitType -> "()"
    | PairType (a,b) -> "("^(show_tuple a b)^")"
    | EmptyRecordType -> "{}"
    | RecordType (_,_,_) as r -> (
      match record_fields r with
      | None, fields -> "{ "^(show_record fields)^" }"
      | Some r, fields -> (show r)^" with { "^(show_record fields)^" }"
    )
    | CaseType (options, t) -> (show_options options)^" -> "^(show t)
    | SumType (options, t) -> show_options options
    | GeneratorType a -> "["^(show a)^"]"
    | ReducerType (a,b,c) -> "Reducer("^(show a)^", "^(show b)^", "^(show c)^")"
    | MappingType (a,b) -> "{" ^ (show a)^" -> "^(show b) ^ "}"
    | ShapeType a -> "Shape("^(show a)^")"
  and show_tuple a = function
    | PairType (b,c) -> (show a)^", "^(show_tuple b c)
    | b -> (show a)^", "^(show b)
  and show_record fields =
    let s,_ = List.fold_left show_field ("","") fields in
    s
  and show_options options =
    match record_options constraints options with
    | None, cases -> "either { "^(show_cases cases)^" }"
    | Some r, cases -> "either { "^(show_cases cases)^" } or "^(show r)
  and show_field (s,sep) (f,f_t) =
    (s ^ sep ^ f ^":" ^ (show f_t), ", ")
  and show_cases cases =
    let s,_ = List.fold_left show_case ("","") cases in
    s
  and show_case (s,sep) (c,c_t) =
    (s ^ sep ^ c ^"(" ^ (show c_t)^")", ", ")
  in show

let is_uppercase c = c = Char.uppercase_ascii c

let is_constructor_name name =
  (String.length name > 0) && (is_uppercase (String.get name 0))

let show_constraint show_type =
  let show_field (s,sep) (f,f_t) =
    (s ^ sep ^ f ^":" ^ (show_type f_t), ", ")
  in
  let show_row expected =
    let s,sep = List.fold_left show_field ("","") expected in
    s ^ sep ^ "..."
  in
  function
    | (x, NoConstraint) -> x
    | (x, RequiredFields fields) ->
      match FieldSet.bindings fields with
      | (f_name, f_type)::_ when is_constructor_name f_name ->
        x ^ " union"
      | fields ->
        x ^ " with { " ^ (show_row fields) ^ " }"

let show = show_expr (fun _ -> NoConstraint) DeBruijn.name
let show_polytype_type_expr t = show_expr (fun x -> DeBruijn.find x t.vars_expr) DeBruijn.name t.type_expr

let show_polytype_constraints =
  let show_genvar (prefix,sep) x =
    (prefix ^ sep ^ (show_constraint show x), ", ")
  in
  let show_genvars = function
    | [] -> ""
    | genvars ->
      let s,_ = List.fold_left show_genvar ("", "") genvars in
      "forall " ^ s ^ ". "
  in
  fun t ->
    let genvar_names = DeBruijn.names t.vars_expr in
    let genvars = List.combine genvar_names t.vars_expr in
    show_genvars genvars

let show_polytype t =
  (show_polytype_constraints t) ^ (show_polytype_type_expr t)

let rewrite_type_expr rewrite_var =
  let rec loop = function
    | TypeVar x -> rewrite_var x
    | TypeSym t -> TypeSym t
    | FunType (t1,t2) ->
      let t1 = loop t1 in
      let t2 = loop t2 in
      FunType (t1, t2)
    | UnitType -> UnitType
    | PairType (t1,t2) ->
      let t1 = loop t1 in
      let t2 = loop t2 in
      PairType (t1, t2)
    | EmptyRecordType -> EmptyRecordType
    | RecordType (r,p,t) ->
      let r = loop r in
      let t = loop t in
      RecordType (r,p,t)
    | CaseType (r,t) ->
      let r = loop r in
      let t = loop t in
      CaseType (r,t)
    | SumType (r,t) ->
      let r = loop r in
      let t = loop t in
      SumType (r,t)
    | GeneratorType t ->
      let t = loop t in
      GeneratorType t
    | ReducerType (t1,t2,t3) ->
      let t1 = loop t1 in
      let t2 = loop t2 in
      let t3 = loop t3 in
      ReducerType (t1, t2, t3)
    | MappingType (t1,t2) ->
      let t1 = loop t1 in
      let t2 = loop t2 in
      MappingType (t1, t2)
    | ShapeType t ->
      let t = loop t in
      ShapeType t
  in loop

let rewrite_type_constraint rewrite_var = function
  | NoConstraint -> NoConstraint
  | RequiredFields fields ->
    let fields = FieldSet.map (rewrite_type_expr rewrite_var) fields in
    RequiredFields fields

let eqn_of_polytype polytype =
  let gen_vars = List.map (fun _ -> Var.new_var ()) polytype.vars_expr in
  let rewrite_var idx = (TypeVar (DeBruijn.find idx gen_vars)) in
  let gen_constraints = List.map (rewrite_type_constraint rewrite_var) polytype.vars_expr in
  {
    vars_eqn = List.map (fun (x,c) -> eqn_of_constraint x c) (List.combine gen_vars gen_constraints);
    type_eqn = rewrite_type_expr rewrite_var polytype.type_expr;
  }

let remove_bound_typevars =
  let rec actual_type x =
    match Var.value x with
    | Some (Type t) -> rewrite_type_expr actual_type t
    | Some (Constraint c) ->
      let c = rewrite_type_constraint actual_type c in
      eqn_of_constraint x c
    | None -> TypeVar x
  in
  rewrite_type_expr actual_type

type var_mapping = var list

let get_var var_mapping x =
  try TypeVar (DeBruijn.find_eq Var.equal var_mapping x)
  with Not_found ->
    (* This type variable is used in the expression but not in the type of the expression.
       It means the sub-expression is unused and can be given any type. *)
    UnitType

let partial_eqn_to_type var_mapping =
  let rec actual_type x =
    match Var.value x with
    | Some (Type t) -> rewrite_type_expr actual_type t
    | _ -> get_var var_mapping x
  in
  rewrite_type_expr actual_type

let type_vars_of_eqn =
  let is_member x = List.exists (Var.equal x) in
  let add_var x vars = if is_member x vars then vars else x::vars in
  let rec add_all_vars x vars =
    match Var.value x with
    | None -> add_var x vars
    | Some (Type t) -> type_vars_expr add_all_vars vars t
    | Some (Constraint c) ->
      let vars = add_var x vars in
      type_vars_constraint add_all_vars vars c
  in
  fun eqn ->
    List.rev (type_vars_expr add_all_vars [] eqn)

let mapping_eqn_to_polytype eqn =
  let eqn = remove_bound_typevars eqn in
  let var_mapping = type_vars_of_eqn eqn in
  let type_expr = partial_eqn_to_type var_mapping eqn in
  let vars_expr =
    List.map (fun x -> match Var.value x with
      | Some (Constraint c) -> rewrite_type_constraint (get_var var_mapping) c
      | _ -> NoConstraint
    ) var_mapping
  in
  ({ vars_expr; type_expr; }, var_mapping)

let eqn_to_polytype eqn =
  fst (mapping_eqn_to_polytype eqn)

let eqn_to_type eqn =
  (eqn_to_polytype eqn).type_expr

let type_error t1 t2 =
  match eqn_to_type (PairType(t1,t2)) with
  | PairType (t1,t2) -> raise (Type_error (t1,t2))
  | _ -> assert false

let missing_fields fields =
  let t = constraint_as_type (RequiredFields fields) in
  let t = eqn_to_type t in
  raise (Missing_fields t)

let check_no_cycle x =
  let rec occurs_in = function
    | Type t -> occurs_in_type t
    | Constraint c -> occurs_in_constraint c
      
  and occurs_in_type = function
    | TypeVar y when Var.is_bound y -> (
      match Var.value y with
      | None -> false
      | Some v -> occurs_in v
    )
    | TypeVar y
    -> Var.equal x y
    | TypeSym _
    | UnitType
    | EmptyRecordType
    -> false
    | FunType (t1,t2)
    | PairType (t1,t2)
    | RecordType (t1,_,t2)
    | CaseType (t1,t2)
    | SumType (t1,t2)
    | MappingType (t1,t2)
    -> occurs_in_type t1 || occurs_in_type t2
    | GeneratorType t
    | ShapeType t
    -> occurs_in_type t
    | ReducerType (t1,t2,t3)
    -> occurs_in_type t1 || occurs_in_type t2 || occurs_in_type t3

  and occurs_in_constraint = function
    | NoConstraint -> false
    | RequiredFields fields ->
      FieldSet.exists (fun _ -> occurs_in_type) fields

  in fun t ->
    if occurs_in t
    then
      match t with
      | Type t -> type_error (TypeVar x) t
      | Constraint c -> type_error (TypeVar x) (constraint_as_type c)
    else true

let rec unify t1 t2 = match t1, t2 with
  | TypeVar x1, TypeVar x2 ->
    Var.merge merge_constrained_types check_no_cycle x1 x2
  | TypeVar x, t | t, TypeVar x ->
    Var.update merge_constrained_types check_no_cycle x (Type t) 
  | t1,t2 ->
    unify_types t1 t2 

and merge_constrained_types t1 t2 = match t1,t2 with
  | Type t1, Type t2 ->
    unify_types t1 t2; Type t1
  | Type t, Constraint c
  | Constraint c, Type t ->
    apply_constraint t c; Type t
  | Constraint c1, Constraint c2 ->
    Constraint (merge_constraints c1 c2)
  
and unify_types t1 t2 = match t1,t2 with
  | TypeVar x, TypeVar y
    when Var.equal x y
  -> ()

  | TypeVar x, t
  | t, TypeVar x
  -> if check_no_cycle x (Type t)
     then Var.assign x (Type t)

  | TypeSym n1, TypeSym n2
  -> if n1 <> n2 then type_error t1 t2

  | FunType (a1,b1), FunType (a2, b2)
  -> unify a1 a2; unify b1 b2

  | UnitType, UnitType
  -> ()

  | PairType (a1,b1), PairType (a2, b2)
  -> unify a1 a2; unify b1 b2

  | EmptyRecordType, EmptyRecordType
  -> ()

  | RecordType (a1,p1,b1), RecordType (a2,p2,b2) when p1 = p2
  -> unify a1 a2; unify b1 b2

  | CaseType (a1,b1), CaseType (a2,b2)
  -> unify a1 a2; unify b1 b2

  | SumType (a1,b1), SumType (a2,b2)
  -> unify a1 a2; unify b1 b2

  | GeneratorType a1, GeneratorType a2
  -> unify a1 a2

  | ReducerType (a1,b1,c1), ReducerType (a2, b2, c2)
  -> unify a1 a2; unify b1 b2; unify c1 c2

  | MappingType (a1,b1), MappingType (a2, b2)
  -> unify a1 a2; unify b1 b2

  | ShapeType a1, ShapeType a2
  -> unify a1 a2

  | _ -> type_error t1 t2

and merge_constraints c1 c2 = match c1,c2 with
  | NoConstraint, c | c, NoConstraint
  -> c

  | RequiredFields f1, RequiredFields f2
  -> let fields =
      FieldSet.merge (fun k of1 of2 -> match of1,of2 with
        | Some f1, Some f2 -> unify f1 f2; of1
        | None, None -> None
        | None, _ -> of2  
        | _, None -> of1
      ) f1 f2
    in
    RequiredFields fields

and apply_constraint t c = match t,c with
  | t, NoConstraint
  -> ()

  | TypeVar x, c
  -> let v =
       match Var.value x with
       | Some t -> merge_constrained_types t (Constraint c)
       | None -> Constraint c
     in
     if check_no_cycle x v
     then Var.set_value x v

  | EmptyRecordType, RequiredFields fields when FieldSet.is_empty fields
  -> ()
  
  | RecordType(_,_,_), RequiredFields fields when FieldSet.is_empty fields
  -> ()
  
  | EmptyRecordType, RequiredFields fields
  -> missing_fields fields

  | RecordType (r,f,f_t), RequiredFields fields
  -> (
    try 
      unify f_t (FieldSet.find f fields);
      let other_fields = RequiredFields (FieldSet.remove f fields) in
      apply_constraint r other_fields
    with Not_found -> 
      apply_constraint r c
  )

  | t, c
  -> type_error t (constraint_as_type c)

let unified t1 t2 =
  unify t1 t2;
  t1

let equal t1 t2 =
  let t1 = eqn_of_polytype t1 in
  let t2 = eqn_of_polytype t2 in
  try unify t1.type_eqn t2.type_eqn; true
  with _ -> false

let rec is_data = function
  | FunType (_,_)
  | ReducerType (_,_,_)
  | CaseType (_,_)
  | SumType (_,_)
  -> false

  | TypeVar _
  | TypeSym _
  | ShapeType _
  | UnitType
  | EmptyRecordType
  -> true

  | GeneratorType t
  -> is_data t

  | PairType (t1, t2)
  | RecordType (t2,_,t1)
  | MappingType (t1, t2)
  -> is_data t1 && is_data t2

let is_abstract t = not (is_data t.type_expr)

let gen_vars t =
  List.map (fun _ -> not (is_data t.type_expr)) t.vars_expr

module Env = struct

  type env = {
    types : polytyp Dict.t;
    non_generic_vars: polyeqn Dict.t;
  }

  let empty = {
    types = Dict.empty;
    non_generic_vars = Dict.empty;
  }

  let set_typeof x t env = {
    env with
    types = Dict.add x t env.types
  }

  let typeof x env =
    Dict.lookup x env.types

  let introduce_var x eqn env =
    let poly_eqn = { vars_eqn = []; type_eqn = eqn } in
    {
      env with
      non_generic_vars = Dict.add x poly_eqn env.non_generic_vars;
    }

  let lookup_var x env =
    match Dict.lookup x env.non_generic_vars with
    | None -> (
      match Dict.lookup x env.types with
      | None -> None
      | Some t -> Some (eqn_of_polytype t)
    )
    | some_t -> some_t
end

let fun_t a b = FunType (a,b)
let pair_t a b = PairType (a,b)

let poly_t n t =
  let rec loop vars n =
    if n = 0 then vars
    else loop (NoConstraint::vars) (n - 1)
  in {
    vars_expr = loop [] n;
    type_expr = t
  }
 
let gen_t x = TypeVar (DeBruijn.of_int x)

let record_t =
  let add_field rec_typ (field_name, field_typ) =
    RecordType (rec_typ, field_name, field_typ)
  in
  List.fold_left add_field EmptyRecordType
