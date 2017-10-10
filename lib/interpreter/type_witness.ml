module Bounded = Series.Bounded
module Reducer = Series.Reducer
module Mapping = Series.Mapping
module Generics = Generics.Repr

open Series.Util

type 'a impl = {
  to_string: 'a -> string;
  of_string: string -> 'a;
  repr: 'a Generics.t;
}

type void

type _ t =
  | UnitSig: unit t
  | NativeSig: string * 'a impl -> 'a t
  | FunSig: 'a t * 'b t -> ('a -> 'b) t
  | PairSig: 'a t * 'b t -> ('a * 'b) t
  | RecordSig: 'a t * string * 'b t -> ('b * 'a) t
  | GeneratorSig: 'a t -> 'a Bounded.producer t
  | ReducerSig: 'a t * 'b t * 'c t -> ('a,'b,'c) Reducer.t t
  | MappingSig: 'a t * 'b t -> ('a,'b) Mapping.t t
  | ShapeSig: 'a t -> 'a Generics.t t
  | VoidSig: void t

type type_sig =
  | Sig: 'a t -> type_sig

type dyn =
  | Dyn: 'a t * 'a -> dyn

type poly_dyn =
  | MonoDyn: 'a t * 'a -> poly_dyn
  | PolyDyn: { make: 'a. 'a t -> poly_dyn } -> poly_dyn

type (_,_) eq = Eq : ('a,'a) eq

type sig_repr =
  | SigRepr: 'a t * 'a Generics.t -> sig_repr

let unit = UnitSig
let lam a b = FunSig(a,b)
let pair a b = PairSig(a,b)
let record a p b = RecordSig(a,p,b)

let add_impl, get_impl =
  let table = Hashtbl.create 53 in
  let add_impl = Hashtbl.replace table in
  let get_impl name =
    try Hashtbl.find table name
    with Not_found -> raise (Type.Unknown_type name)
  in
  (add_impl, get_impl)                                                                                                                                                                  

let dyn_of_literal lit_typ literal = match lit_typ with
  | Type.TypeSym name -> (
    match get_impl name with
    | Sig (NativeSig(_,impl) as lit_sig) -> Dyn (lit_sig, impl.of_string literal)
    | _ -> raise (Semantics.Compilation_error ("Not a primitive native type: "^name))
  )
  | _ -> raise (Semantics.Compilation_error ("Not a native type: "^(Type.show lit_typ)))

let rec sig_repr_of_shape = function
  | Datatype.Atomic name -> (
    match get_impl name with
    | Sig (NativeSig(_,impl) as impl_sig) -> SigRepr (impl_sig, impl.repr)
    | _ -> raise (Semantics.Compilation_error ("Not a primitive native type: "^name))
  )

  | Datatype.Tuple [] ->  SigRepr(UnitSig, Generics.unit)
  | Datatype.Tuple [s] -> sig_repr_of_shape s
  | Datatype.Tuple (s::ss) ->
    let SigRepr (tuple_sig, tuple_repr) = sig_repr_of_tuple s ss in
    SigRepr (tuple_sig, Generics.tuple tuple_repr)

  | Datatype.Record fields ->
    let empty_record_sig_repr = SigRepr(UnitSig, Generics.empty_record) in
    let SigRepr (record_sig, record_repr) = List.fold_left sig_repr_of_field empty_record_sig_repr fields in
    SigRepr (record_sig, Generics.record record_repr)

  | Datatype.List item_s ->
    let SigRepr (item_sig, item_repr) = sig_repr_of_shape item_s in
    SigRepr (GeneratorSig item_sig, Generics.list item_repr)

and sig_repr_of_tuple s1 = function
  | [] -> sig_repr_of_shape s1
  | s2::ss ->
    let SigRepr (head_sig, head_repr) = sig_repr_of_shape s1 in
    let SigRepr (tail_sig, tail_repr) = sig_repr_of_tuple s2 ss in
    SigRepr (PairSig (head_sig,tail_sig), Generics.pair head_repr tail_repr)

and sig_repr_of_field r (f_name,f) =
  let SigRepr (r_sig, r_repr) = r in
  let SigRepr (f_sig, f_repr) = sig_repr_of_shape f in
  SigRepr (RecordSig (r_sig, f_name, f_sig), Generics.((field f_name f_repr) & r_repr))

let dyn_of_shape shape =
  let SigRepr (s_sig, s_repr) = sig_repr_of_shape shape in
  Dyn (ShapeSig s_sig, s_repr)

let rec gen_val_of_sig: type a. a t -> a Generics.t option
  = function
  | UnitSig -> Some Generics.unit
  | NativeSig (_, impl) -> Some impl.repr
  | PairSig (a,b) -> Option.(
    gen_val_of_sig a
    >>= fun a_repr ->
    gen_val_of_sig b
    >|= fun b_repr ->
    Generics.pair a_repr b_repr
  )
  | RecordSig (r, name, f) -> Option.(
    gen_val_of_sig f
    >>= fun f_repr ->
    gen_val_of_sig r
    >|= fun r_repr ->
    Generics.((field name f_repr) & r_repr)
  )
  | GeneratorSig a -> Option.(
    gen_val_of_sig a
    >|= fun a_repr ->
    Generics.list a_repr
  )
  | MappingSig (k,v) -> Option.(
    gen_val_of_sig k
    >>= fun k_repr ->
    gen_val_of_sig v
    >|= fun v_repr ->
    Generics.mapping k_repr v_repr
  )
  | FunSig (_,_) -> None
  | ReducerSig (_,_,_) -> None
  | ShapeSig _ -> None
  | VoidSig -> None

external identity: 'a -> 'b = "%identity"

let rec eq_type : type a b. a t -> b t -> (a,b) eq option =
  fun a b -> match a, b with
  | NativeSig (a_name,a_impl), NativeSig (b_name,b_impl)
    (* Here, we use the identity function
       to force the compiler accept that a and b are sentinels for the same type
       since they have the *same* implementation.
       This is done so the list of type witness is extensible. *)
    when a_name = b_name && a_impl == identity b_impl 
    -> identity (Some Eq)

  | UnitSig, UnitSig -> Some Eq
  | PairSig(a1,b1), PairSig(a2,b2) -> (
    match eq_type a1 a2, eq_type b1 b2 with
    | Some Eq, Some Eq -> Some Eq
    | _ -> None
  )
  | FunSig(a1,b1), FunSig(a2,b2) -> (
    match eq_type a1 a2, eq_type b1 b2 with
    | Some Eq, Some Eq -> Some Eq
    | _ -> None
  )
  | RecordSig(a1,p1,b1), RecordSig(a2,p2,b2) when p1 = p2 -> (
    match eq_type a1 a2, eq_type b1 b2 with
    | Some Eq, Some Eq -> Some Eq
    | _ -> None
  )
  | GeneratorSig a1, GeneratorSig a2 -> (
    match eq_type a1 a2 with
   | Some Eq -> Some Eq
   | None -> None
  )
  | ReducerSig(a1,b1,c1), ReducerSig(a2,b2,c2) -> (
    match eq_type a1 a2, eq_type b1 b2, eq_type c1 c2 with
    | Some Eq, Some Eq, Some Eq -> Some Eq
    | _ -> None
  )
  | MappingSig(a1,b1), MappingSig(a2,b2) -> (
    match eq_type a1 a2, eq_type b1 b2 with
    | Some Eq, Some Eq -> Some Eq
    | _ -> None
  )
  | ShapeSig a1, ShapeSig a2 -> (
    match eq_type a1 a2 with
    | Some Eq -> Some Eq
    | _ -> None
  )
  | VoidSig, VoidSig -> Some Eq
  | _ -> None

let rec show: type a. a t -> string = function
  | NativeSig (t,_) -> t
  | UnitSig -> "()"
  | VoidSig -> "Void"
  | FunSig(a,b) -> Format.sprintf "(%s->%s)" (show a) (show b) 
  | PairSig(a,b) -> Format.sprintf "(%s,%s)" (show a) (show b) 
  | RecordSig(a,p,b) -> Format.sprintf "%s with {%s:%s}" (show a) p (show b) 
  | GeneratorSig a -> Format.sprintf "[%s]" (show a) 
  | ReducerSig(a,b,c) -> Format.sprintf "Reducer(%s, %s, %s)" (show a) (show b) (show c)
  | MappingSig(a,b) -> Format.sprintf "{%s->%s}" (show a) (show b) 
  | ShapeSig(a) -> Format.sprintf ": %s" (show a)

let top = fst
let pop_than f (_,s) = f s
let id x = x

type _ field_getter =
  FieldGetter: ('a -> 'b) * 'b t -> 'a field_getter

let rec field_getter: type a. string -> a t -> a field_getter =
  fun p r_sig ->
  match r_sig with
  | RecordSig(_, f, f_sig) when f = p ->
    FieldGetter (top, f_sig)

  | RecordSig(b_sig, _, _) -> (
    let FieldGetter (get, p_sig) = field_getter p b_sig in
    FieldGetter (pop_than get, p_sig)
  )
  | _ -> raise Not_found

let rec eval_type env = function
  | Type.TypeVar v -> lookup_type env v

  | Type.UnitType -> Sig UnitSig
  | Type.TypeSym t -> get_impl t

  | Type.FunType (a,b) ->
    let Sig a_t = eval_type env a in
    let Sig b_t = eval_type env b in
    Sig (FunSig(a_t, b_t))

  | Type.PairType (a,b) ->
    let Sig a_t = eval_type env a in
    let Sig b_t = eval_type env b in
    Sig (PairSig(a_t, b_t))

  | Type.EmptyRecordType ->
    Sig UnitSig

  | Type.RecordType (_,_,_) as r -> (
    match Type.record_fields r with
    | None, fields ->
      List.fold_left (eval_field_type env) (Sig UnitSig) fields
    | Some t, fields ->
      List.fold_left (eval_field_type env) (eval_type env t) fields
  )

  | Type.CaseType (cases, res) ->
    (* The result type is only used during type checking. *)
    eval_type env cases

  | Type.SumType (cases, res) -> (
    let Sig cases_t = eval_type env cases in
    let Sig res_t = eval_type env res in
    Sig (FunSig(cases_t, res_t))
  )

  | Type.GeneratorType a -> (
    let Sig a_t = eval_type env a in
    Sig (GeneratorSig a_t)
  )

  | Type.ReducerType (a,b,c) ->
    let Sig a_t = eval_type env a in
    let Sig b_t = eval_type env b in
    let Sig c_t = eval_type env c in
    Sig (ReducerSig(a_t, b_t, c_t))

  | Type.MappingType (a,b) ->
    let Sig a_t = eval_type env a in
    let Sig b_t = eval_type env b in
    Sig (MappingSig(a_t, b_t))

  | Type.ShapeType a ->
    let Sig a_t = eval_type env a in
    Sig (ShapeSig a_t)

and lookup_type env v = match DeBruijn.index v,env with
  | DeBruijn.VZ,   t::_   -> t
  | DeBruijn.VS v, _::env -> lookup_type env v
  | _, [] -> raise (Semantics.Compilation_error ("Compilation of an open type expression"))

and eval_field_type env (Sig r_sig) (f,f_t) =
  let Sig(f_sig) = eval_type env f_t in
  Sig (RecordSig(r_sig, f, f_sig))

let rec specialize: poly_dyn -> type_sig list -> dyn
  = fun poly_dyn type_args -> match poly_dyn, type_args with
  | MonoDyn (t,x), [] -> Dyn (t,x)
  | PolyDyn t, (Sig type_arg)::type_args -> specialize (t.make type_arg) type_args 
  | MonoDyn (t,x), _ ->
   (* We don't raise (Semantics.Compilation_error ("Unexpected type argument")),
      because of the optimisation in compiler.compile
      which replaces the useless generic types by void values. *)
    Dyn (t,x)
  | PolyDyn _, _ -> raise (Semantics.Compilation_error ("Missing type argument"))

let specialize_polydyn type_params (poly_dyn, type_args) =
  let type_args = List.map (eval_type type_params) type_args in
  specialize poly_dyn type_args

let sub_type_expr t sub_t =
  Type.{ t with type_expr = sub_t }

type 'a show = Show : ('a -> string) -> 'a show

let rec show_dyn: type a. Type.polytyp -> a t -> a show  =
  fun x_t x_sig -> match x_sig with
  | NativeSig(_,impl) -> Show (impl.to_string)
  | UnitSig -> Show (fun () -> "()")
  | FunSig(_,_) -> Show (fun _ -> Type.show_polytype_type_expr x_t)
  | ReducerSig (_,_,_) -> Show (fun _ -> Type.show_polytype_type_expr x_t)
  | ShapeSig _ -> Show (fun _ -> Type.show_polytype_type_expr x_t)
  | GeneratorSig item_sig -> (
    match x_t.Type.type_expr with
    | Type.GeneratorType item_t ->
      let item_t = sub_type_expr x_t item_t in
      let Show show_item = show_dyn item_t item_sig in
      Show (Bounded.(show_first 100 "[" ", " "..." "]" show_item))
    | _ ->
      (* A GeneratorSig can only be the implementation of a GeneratorType. *)
      assert false
  )
  | MappingSig(_,_) -> Show (fun _ -> "<mapping>") (* FIXME *)
  | PairSig (a_sig, b_sig) -> (
    match x_t.Type.type_expr with
    | Type.PairType (a_t, b_t) ->
      let a_t = sub_type_expr x_t a_t in
      let b_t = sub_type_expr x_t b_t in
      let Show show = show_tuple a_t a_sig b_t b_sig in
      Show (fun p -> "("^( show p )^")")
    | _ ->
      (* A PairSig can only be the implementation of a PairType. *)
      assert false
  )
  | RecordSig (a_sig, b_name, b_sig) -> (
    match x_t.Type.type_expr with
    | Type.RecordType (a_t, _, b_t) ->
      let a_t = sub_type_expr x_t a_t in
      let b_t = sub_type_expr x_t b_t in
      let Show show = show_record b_name b_t b_sig a_t a_sig in
      Show (fun p -> "{"^( show p )^"}")
    | _ ->
      (* A RecordSig can only be the implementation of a RecordType. *)
      assert false
  )
  | VoidSig ->
    (* No concrete value has type void,
       but a show_void function is required for polymorphic expressions (like `[]`)
       where the polymorphic type is not used. *)
    Show (fun _ -> assert false)

and show_tuple: type a b. Type.polytyp -> a t -> Type.polytyp -> b t -> (a*b) show =
  fun a_t a_sig x_t x_sig ->
    let Show show_a = show_dyn a_t a_sig in
    match x_sig, x_t.Type.type_expr with
    | PairSig(b_sig, c_sig), Type.PairType (b_t, c_t) ->
      let b_t = sub_type_expr x_t b_t in
      let c_t = sub_type_expr x_t c_t in
      let Show show_x = show_tuple b_t b_sig c_t c_sig in
      Show (fun (a,x) -> (show_a a)^", "^(show_x x))
    | _, _ ->
      let Show show_x = show_dyn x_t x_sig in
      Show (fun (a,x) -> (show_a a)^", "^(show_x x))

and show_record: type a b. string -> Type.polytyp -> a t -> Type.polytyp -> b t -> (a*b) show =
  fun a_name a_t a_sig x_t x_sig ->
    let Show show_a = show_dyn a_t a_sig in
    match x_sig, x_t.Type.type_expr with
    | RecordSig(c_sig, b_name, b_sig), Type.RecordType (c_t, _, b_t) ->
      let b_t = sub_type_expr x_t b_t in
      let c_t = sub_type_expr x_t c_t in
      let Show show_x = show_record b_name b_t b_sig c_t c_sig in
      Show (fun (a,x) -> (show_x x)^", "^a_name^" = "^(show_a a))
    | _, _ ->
      Show (fun (a,_) -> a_name^" = "^(show_a a))

let show_poly_dyn polytype =
  if Type.is_abstract polytype
  then
    fun _ -> (":", Type.show_polytype polytype)
  else
    function
    | MonoDyn (t,x) ->
      let Show show = show_dyn polytype t in
      ("=", show x)
    | _ -> assert false

  (* si PolyDyn -> affiche le type *)
  (* si PolyDyn of caseType -> affiche une valeur calculée in-situ *)
  (* si MonoDyn et data -> affiche la valeur *)
  (* si MonoDyn non data -> affiche le type *)

