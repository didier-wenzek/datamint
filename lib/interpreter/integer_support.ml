module Add(C: sig
  include Compiler.S
  include Compiler.With_bool_support
end) : sig
  include Compiler.S
  include Compiler.With_bool_support
  include Compiler.With_int_support
end = struct

  include C
  module Collection = Series.Bounded
  
  type int_repr = int

  let int_t = Type.TypeSym "Int"
  let int_sig = Type_witness.(NativeSig("Int", {
    to_string = string_of_int;
    of_string = int_of_string;
    repr = Generics.Repr.int;
  }))
  let _ = Type_witness.(add_impl "Int" (Sig int_sig))
  
  let int_of_repr x = x
  let repr_of_int x = x

  let poly_t = Type.poly_t
  let fun_t = Type.fun_t

  let int_value n =
    (poly_t 0 int_t, Type_witness.(MonoDyn (int_sig, n)))

  let int_operator op =
    let op_type = poly_t 0 (fun_t int_t (fun_t int_t int_t)) in
    let op_sig = Type_witness.(FunSig(int_sig, FunSig(int_sig,int_sig))) in
    (op_type, Type_witness.MonoDyn (op_sig, op))
    
  let range =
    let op_type = poly_t 0 (fun_t int_t (fun_t int_t (Type.GeneratorType int_t))) in
    let op_sig = Type_witness.(FunSig(int_sig, FunSig(int_sig, GeneratorSig int_sig))) in
    (op_type, Type_witness.MonoDyn (op_sig, Collection.range))

  let take =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t int_t (fun_t (GeneratorType a) (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(int_sig,FunSig(GeneratorSig a,GeneratorSig a)), Collection.take)
    })

  let drop =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t int_t (fun_t (GeneratorType a) (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(int_sig,FunSig(GeneratorSig a,GeneratorSig a)), Collection.drop)
    })

  let repeat =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t int_t (fun_t a (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(int_sig,FunSig(a,GeneratorSig a)), Collection.repeat)
    })

  let iterate =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t int_t (fun_t (fun_t a a) (fun_t a (GeneratorType a))))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(int_sig,FunSig(FunSig(a,a),FunSig(a,GeneratorSig a))), Collection.iterate)
    })

  let time_ms =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let t = Type.(poly_t 2 (fun_t (fun_t a b) (fun_t a (pair_t int_t b)))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b ->
      MonoDyn(FunSig(FunSig(a,b),FunSig(a,PairSig(int_sig,b))), Series.Util.Time.time_ms)
    }})
   
(* The type a reducer can no more be abstract:
   - this type is required to persist the state of a reduce operation.
   - this type is now exposed to the type system.

  let top =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let k = Type.gen_t 2 in
    let m = a Sift_reducer.t
    let t = Type.(poly_t 3 (fun_t (fun_t a k) (fun_t (fun_t a b) (fun_t int_t (ReducerType (a,m,GeneratorType a)))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun k -> PolyDyn { make = fun m ->
      MonoDyn(FunSig(FunSig(a,k),FunSig(FunSig(a,b),FunSig(int_sig,ReducerSig(a,m,GeneratorSig a)))), Sift_top.reducer)
    }}}})
*)

  let initial_env = initial_env
    |> define "zero" (int_value 0)
    |> define "one" (int_value 1)
    |> define "add" (int_operator (+))
    |> define "mult" (int_operator ( * ))
    |> define "+" (int_operator (+))
    |> define "*" (int_operator ( * ))
    |> define "-" (int_operator (-))
    |> define "/" (int_operator (/))
    |> define "mod" (int_operator (mod))
    |> define "range" range
    |> define "take" take
    |> define "drop" drop
    |> define "repeat" repeat
    |> define "times" repeat
    |> define "iterate" iterate
    |> define "time_ms" time_ms
    (* |> define "top" top *)

  let _ = Parsing_helper.(List.iter add_operator [
     "+", LeftAssoc, 6;
     "-", LeftAssoc, 6;
     "*", LeftAssoc, 7;
     "/", LeftAssoc, 7;
     "mod", LeftAssoc, 7;
     "times", NonAssoc, 8;
  ])
end
