module Collection = Series.Bounded

module Add(C: sig
  include Compiler.S
  include Compiler.With_bool_support
  include Compiler.With_int_support
  include Compiler.With_string_support
end) : sig
  include Compiler.S
  include Compiler.With_bool_support
  include Compiler.With_int_support
  include Compiler.With_string_support
end = struct

  include C

  let empty_sequence =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (GeneratorType a)) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(GeneratorSig a, Collection.empty)
    })

  let cons =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t a (fun_t (GeneratorType a) (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(a,FunSig(GeneratorSig a,GeneratorSig a)), Collection.cons)
    })

  let sonc =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (GeneratorType a) (fun_t a (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(GeneratorSig a,FunSig(a,GeneratorSig a)), Collection.sonc)
    })

  let append =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (GeneratorType a) (fun_t (GeneratorType a) (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(GeneratorSig a,FunSig(GeneratorSig a,GeneratorSig a)), Collection.append)
    })

  let concat =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (GeneratorType (GeneratorType a)) (GeneratorType a))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(GeneratorSig (GeneratorSig a),GeneratorSig a), Collection.concat)
    })

  let reduce =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let t = Type.(poly_t 3 (fun_t (ReducerType (a,b,c)) (fun_t (GeneratorType a) c))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c ->
      MonoDyn(FunSig(ReducerSig (a,b,c), FunSig(GeneratorSig a, c)), Collection.reduce)
    }}})

  let map =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let t = Type.(poly_t 2 (fun_t (FunType (a,b)) (fun_t (GeneratorType a) (GeneratorType b)))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b ->
      MonoDyn(FunSig(FunSig (a,b), FunSig(GeneratorSig a, GeneratorSig b)), Collection.map)
    }})

  let flat_map =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let t = Type.(poly_t 2 (fun_t (FunType (a,GeneratorType b)) (fun_t (GeneratorType a) (GeneratorType b)))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b ->
      MonoDyn(FunSig(FunSig (a,GeneratorSig b), FunSig(GeneratorSig a, GeneratorSig b)), Collection.flat_map)
    }})

  let unnest =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let t = Type.(poly_t 2 (fun_t (FunType (a,GeneratorType b)) (fun_t (GeneratorType a) (GeneratorType (PairType (b,a)))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b ->
      MonoDyn(FunSig(FunSig (a,GeneratorSig b), FunSig(GeneratorSig a, GeneratorSig (PairSig (b,a)))), Collection.unnest)
    }})

  let mapping =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let d = Type.gen_t 3 in
    let t = Type.(poly_t 4 (fun_t (FunType (a,b)) (fun_t (ReducerType (b,c,d)) (ReducerType (a,c,d))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c -> PolyDyn { make = fun d ->
      MonoDyn(FunSig(FunSig (a,b), FunSig(ReducerSig (b,c,d), ReducerSig (a,c,d))), Reducer.map)
    }}}})

  let flat_mapping =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let d = Type.gen_t 3 in
    let t = Type.(poly_t 4 (fun_t (FunType (a,GeneratorType b)) (fun_t (ReducerType (b,c,d)) (ReducerType (a,c,d))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c -> PolyDyn { make = fun d ->
      MonoDyn(FunSig(FunSig (a,GeneratorSig b), FunSig(ReducerSig (b,c,d), ReducerSig (a,c,d))), Reducer.flat_map Collection.iter)
    }}}})

  let unnesting =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let d = Type.gen_t 3 in
    let t = Type.(poly_t 4 (fun_t (FunType (a,GeneratorType b)) (fun_t (ReducerType (PairType (b,a),c,d)) (ReducerType (a,c,d))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c -> PolyDyn { make = fun d ->
      MonoDyn(FunSig(FunSig (a,GeneratorSig b), FunSig(ReducerSig (PairSig(b,a),c,d), ReducerSig (a,c,d))), Reducer.unnest Collection.iter)
    }}}})

  let monoid =
    let a = Type.gen_t 0 in
    let op = Type.(fun_t a (fun_t a a)) in
    let t = Type.(poly_t 1 (fun_t a (fun_t op (ReducerType (a,a,a))))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      let op = FunSig(a,FunSig(a,a)) in
      MonoDyn(FunSig(a, FunSig(op, ReducerSig(a,a,a))), Reducer.monoid)
    })

  let monoid_with_maximum =
    let a = Type.gen_t 0 in
    let op = Type.(fun_t a (fun_t a a)) in
    let t = Type.(poly_t 1 (fun_t a (fun_t op (fun_t a (ReducerType (a,a,a)))))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      let op = FunSig(a,FunSig(a,a)) in
      MonoDyn(FunSig(a, FunSig(op, FunSig(a,ReducerSig(a,a,a)))), Reducer.monoid_with_maximum)
    })

  let dedupe =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (GeneratorType a) (GeneratorType a))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(GeneratorSig a,GeneratorSig a), Collection.dedupe)
    })

  let unique =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (GeneratorType a) (GeneratorType a))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(GeneratorSig a,GeneratorSig a), Collection.unique)
    })

  let interpose =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t a (fun_t (GeneratorType a) (GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(a,FunSig(GeneratorSig a,GeneratorSig a)), Collection.interpose)
    })

  let project =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let d = Type.gen_t 3 in
    let t = Type.(poly_t 4 (fun_t (FunType(a,b)) (fun_t (ReducerType(c,d,a)) (ReducerType(c,d,b))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c -> PolyDyn { make = fun d ->
      MonoDyn(FunSig(FunSig(a,b), FunSig(ReducerSig(c,d,a), ReducerSig(c,d,b))), Reducer.project)
    }}}})

  let union_reducer =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let t = Type.(poly_t 3 (fun_t (ReducerType(a,b,c)) (ReducerType(GeneratorType a,b,c)))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c ->
      MonoDyn(FunSig(ReducerSig(a,b,c), ReducerSig(GeneratorSig a, b, c)), Collection.union_reducer)
    }}})
    
  let product_reducer =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let d = Type.gen_t 3 in
    let e = Type.gen_t 4 in
    let f = Type.gen_t 5 in
    let t = Type.(poly_t 6 (fun_t (ReducerType(a,b,c)) (fun_t (ReducerType(d,e,f)) (ReducerType(PairType(a,d),PairType(b,e),PairType (c,f)))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c -> PolyDyn { make = fun d -> PolyDyn { make = fun e -> PolyDyn { make = fun f ->
      MonoDyn(FunSig(ReducerSig(a,b,c), FunSig(ReducerSig(d,e,f), ReducerSig(PairSig(a,d), PairSig(b,e), PairSig(c,f)))), Reducer.product)
    }}}}}})
    
(*
  let group_reducer =
    let a = Type.gen_t 0 in
    let k = Type.gen_t 1 in
    let v = Type.gen_t 2 in
    let b = Type.gen_t 3 in
    let key = Type.fun_t a k in
    let value = Type.fun_t a v in
    let red = Type.ReducerType(v,b) in
    let grp = Type.(record_t [
      "keys", GeneratorType k;
      "pairs", GeneratorType (PairType(k,b));
      "values", FunType (k, GeneratorType b);
    ]) in
    let grp_red = Type.(ReducerType(a, grp)) in
    let t = Type.(poly_t 4 (fun_t key (fun_t value (fun_t red grp_red)))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun k -> PolyDyn { make = fun v -> PolyDyn { make = fun b ->
      let key = FunSig (a,k) in
      let value = FunSig (a,v) in
      let red = ReducerSig (v,b) in
      let grp = RecordSig ( RecordSig ( RecordSig (UnitSig,
        "keys", GeneratorSig k),
        "pairs", GeneratorSig (PairSig(k,b))),
        "values", FunSig (k, GeneratorSig b))
      in
      let grp_red = ReducerSig (a,grp) in
      let to_mapping m =
        Mapping.(values m, (pairs m, (keys m, ())))
      in
      let group_reducer k v r =
        let gr = Collection.group_reducer k v r in
        Collection.project to_mapping gr
      in
      MonoDyn(FunSig(key, FunSig(value, FunSig(red, grp_red))), group_reducer)
    }}}})

  let group_updates =
    let a = Type.gen_t 0 in
    let k = Type.gen_t 1 in
    let v = Type.gen_t 2 in
    let w = Type.gen_t 3 in
    let b = Type.gen_t 4 in
    let key = Type.fun_t a k in
    let value = Type.fun_t a v in
    let red = Type.ReducerType(v,w) in
    let update = Type.(fun_t (pair_t k w) b) in
    let t = Type.(poly_t 5 (fun_t key (fun_t value (fun_t red (fun_t update (fun_t update (fun_t (GeneratorType a) (GeneratorType b)))))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun k -> PolyDyn { make = fun v -> PolyDyn { make = fun w -> PolyDyn { make = fun b ->
      let key = FunSig(a,k) in
      let value = FunSig(a,v) in
      let red = ReducerSig(v,w) in
      let update = FunSig(PairSig(k,w),b) in
      MonoDyn(FunSig(key, FunSig(value, FunSig(red, FunSig(update, FunSig(update, FunSig(GeneratorSig a,GeneratorSig b)))))),
        Collection.group_updates)
    }}}}})
*)

  let take_first =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (GeneratorType a) (GeneratorType a))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(GeneratorSig a,GeneratorSig a), Collection.take_first)
    })

  let take_last =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (GeneratorType a) (GeneratorType a))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(GeneratorSig a,GeneratorSig a), Collection.take_last)
    })

(*
  let first_or =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t a (ReducerType(a,a,a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(a,ReducerSig(a,a,a)), Reducer.first_or)
    })

  let last_or =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t a (ReducerType(a,a,a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(a,ReducerSig(a,a,a)), Reducer.last_or)
    })
*)

  let rolling =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let t = Type.(poly_t 3 (fun_t (ReducerType(a,b,c)) (fun_t (GeneratorType a) (GeneratorType c)))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c ->
      MonoDyn(FunSig(ReducerSig(a,b,c),FunSig(GeneratorSig a,GeneratorSig c)), Collection.rolling)
    }}})

  let generator =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let t = Type.(poly_t 2 (fun_t a (fun_t (fun_t a (PairType (a,b))) (GeneratorType b)))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b ->
      MonoDyn(FunSig(a,FunSig(FunSig(a,PairSig(a,b)), GeneratorSig b)), Collection.generator)
    }})

  let transduce =
    let a = Type.gen_t 0 in
    let b = Type.gen_t 1 in
    let c = Type.gen_t 2 in
    let t = Type.(poly_t 3 (fun_t a (fun_t (fun_t b (fun_t a (PairType (a, GeneratorType c)))) (fun_t (fun_t a (GeneratorType c)) (fun_t (GeneratorType b) (GeneratorType c)))))) in
    Type_witness.(t, PolyDyn { make = fun a -> PolyDyn { make = fun b -> PolyDyn { make = fun c ->
      MonoDyn(
        FunSig(a, FunSig(FunSig (b, FunSig(a,PairSig(a,GeneratorSig c))), FunSig (FunSig (a, GeneratorSig c), FunSig(GeneratorSig b, GeneratorSig c)))),
        fun seed next term -> Collection.transduce seed next term None)
    }}})
    
  let initial_env = initial_env
    |> define "empty_sequence" empty_sequence
    |> define "cons" cons
    |> define "sonc" sonc
    |> define "append" append
    |> define "concat" concat
    |> define "reduce" reduce
    |> define "map" map
    |> define "flat_map" flat_map
    |> define "unnest" unnest
    |> define "mapping" mapping
    |> define "flat_mapping" flat_mapping
    |> define "unnesting" unnesting
    |> define "monoid" monoid
    |> define "monoid_with_maximum" monoid_with_maximum
    |> define "unique" unique
    |> define "interpose" interpose
    |> define "dedupe" dedupe
    |> define "project" project
    |> define "union_reducer" union_reducer
    |> define "product_reducer" product_reducer
(*  |> define "group_reducer" group_reducer
    |> define "group_updates" group_updates  *)
    |> define "take_first" take_first
    |> define "take_last" take_last
(*  |> define "first_or" first_or
    |> define "last_or" last_or *)
    |> define "rolling" rolling
    |> define "generator" generator
    |> define "transduce" transduce
end
