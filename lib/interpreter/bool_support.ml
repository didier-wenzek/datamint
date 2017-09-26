module Collection = Series.Bounded

module Add(C: Compiler.S) : sig
  include Compiler.S
  include Compiler.With_bool_support
end = struct
  include C
  
  type bool_repr = bool

  let bool_t = Type.TypeSym "Bool"
  let bool_sig = Type_witness.(NativeSig("Bool", {
    to_string = string_of_bool;
    of_string = bool_of_string;
    repr = Generics.bool;
  }))
  let _ = Type_witness.(add_impl "Bool" (Sig bool_sig))

  let bool_of_repr x = x
  let repr_of_bool x = x
  let adapt_predicate f = f

  let unit_t = Type.UnitType
  let poly_t = Type.poly_t
  let fun_t = Type.fun_t
  let gen_t = Type.gen_t

  let bool_value n =
    (poly_t 0 bool_t, Type_witness.(MonoDyn (bool_sig, n)))

  let bool_fun f =
    let op_type = poly_t 0 (fun_t bool_t bool_t) in
    let op_sig = Type_witness.(FunSig(bool_sig,bool_sig)) in
    (op_type, Type_witness.MonoDyn (op_sig, f))
    
  let bool_operator op =
    let op_type = poly_t 0 (fun_t bool_t (fun_t bool_t bool_t)) in
    let op_sig = Type_witness.(FunSig(bool_sig, FunSig(bool_sig,bool_sig))) in
    (op_type, Type_witness.MonoDyn (op_sig, op))

  let if_then_else =
    let a = gen_t 0 in
    let lazy_a = fun_t unit_t a in
    let t = poly_t 1 (fun_t bool_t (fun_t lazy_a (fun_t lazy_a a))) in
    let if_then_else x t f = if x then t () else f () in
    Type_witness.(t, PolyDyn { make = fun a ->
      let lazy_a = FunSig(UnitSig,a) in
      MonoDyn(FunSig(bool_sig, FunSig(lazy_a, FunSig(lazy_a ,a))), if_then_else)
    })

  type gen_comparator = { comp:'a. 'a -> 'a -> bool }
  let comp_operator op =
    let a = gen_t 0 in
    let t = poly_t 1 (fun_t a (fun_t a bool_t)) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(a, FunSig(a, bool_sig)), op.comp)
    })

  let filter =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (FunType (a,bool_t)) (fun_t (GeneratorType a) (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(FunSig (a,bool_sig), FunSig(GeneratorSig a, GeneratorSig a)), Collection.filter)
    })

  let filtering =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let t = Type.(poly_t 3 (fun_t (FunType (a,bool_t)) (fun_t (ReducerType (a,b,c)) (ReducerType (a,b,c))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c ->
      MonoDyn(FunSig(FunSig (a,bool_sig), FunSig(ReducerSig (a,b,c), ReducerSig (a,b,c))), Reducer.filter)
    }}})

  let take_while =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (FunType (a,bool_t)) (fun_t (GeneratorType a) (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(FunSig (a,bool_sig),FunSig(GeneratorSig a,GeneratorSig a)), Collection.take_while)
    })
    
  let drop_while =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (FunType (a,bool_t)) (fun_t (GeneratorType a) (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(FunSig (a,bool_sig),FunSig(GeneratorSig a,GeneratorSig a)), Collection.drop_while)
    })

  let bounded_generator =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let t = Type.(poly_t 2 (fun_t a (fun_t (fun_t a (PairType (a,b))) (fun_t (fun_t a bool_t) (GeneratorType b))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b ->
      MonoDyn(FunSig(a,FunSig(FunSig(a,PairSig(a,b)), FunSig(FunSig(a,bool_sig), GeneratorSig b))), Collection.bounded_generator)
    }})

  let monoid_with_maximum_check =
    let a = Type.gen_t 0 in
    let op = Type.(fun_t a (fun_t a a)) in
    let t = Type.(poly_t 1 (fun_t a (fun_t op (fun_t (fun_t a bool_t) (ReducerType (a,a,a)))))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      let op = FunSig(a,FunSig(a,a)) in
      MonoDyn(FunSig(a, FunSig(op, FunSig(FunSig(a,bool_sig),ReducerSig(a,a,a)))), Reducer.monoid_with_maximum_check)
    })

  let initial_env = initial_env
    |> define "true" (bool_value true)
    |> define "false" (bool_value false)
    |> define "not" (bool_fun not)
    |> define "and" (bool_operator (&&))
    |> define "or" (bool_operator (||))
    |> define "if_then_else" if_then_else
    |> define "==" (comp_operator { comp = (=) })
    |> define "!=" (comp_operator { comp = (<>) })
    |> define "<" (comp_operator { comp = (<) })
    |> define ">" (comp_operator { comp = (>) })
    |> define "<=" (comp_operator { comp = (<=) })
    |> define ">=" (comp_operator { comp = (>=) })
    |> define "filter" filter
    |> define "filtering" filtering
    |> define "take_while" take_while
    |> define "drop_while" drop_while
    |> define "bounded_generator" bounded_generator
    |> define "monoid_with_maximum_check" monoid_with_maximum_check

  let _ = Parsing_helper.(List.iter add_operator [
    "or",  LeftAssoc, 1;
    "and", LeftAssoc, 2;
    "==", NonAssoc, 4;
    "!=", NonAssoc, 4;
    "<", NonAssoc, 4;
    ">", NonAssoc, 4;
    "<=", NonAssoc, 4;
    ">=", NonAssoc, 4;
  ])
end
