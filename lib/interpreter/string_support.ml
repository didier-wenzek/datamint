module Serialization = Generics.Serialization
module Json_support = Generics.Json_support
module Marshal_support = Generics.Marshal_support

module Add(C: sig
  include Compiler.S
  include Compiler.With_bool_support
  include Compiler.With_int_support
end) : sig
  include Compiler.S
  include Compiler.With_bool_support
  include Compiler.With_int_support
  include Compiler.With_string_support
end = struct
  include C
  
  let id x = x
  let escape s =
    let b = Buffer.create 80 in
    let l = String.length s in
    Buffer.add_char b '"';
    for i = 0 to l - 1 do
      match s.[i] with
      | '"'  -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c -> Buffer.add_char b c
    done;
    Buffer.add_char b '"';
    Buffer.contents b

  type string_repr = string

  let string_t = Type.TypeSym "String"
  let string_sig = Type_witness.(NativeSig("String", {
    to_string = escape;
    of_string = id;
    repr = Generics.Repr.string;
  }))
  let _ = Type_witness.(add_impl "String" (Sig string_sig))

  let string_of_repr x = x
  let repr_of_string x = x

  let poly_t = Type.poly_t
  let fun_t = Type.fun_t

  let string_operator op =
    let op_type = poly_t 0 (fun_t string_t (fun_t string_t string_t)) in
    let op_sig = Type_witness.(FunSig(string_sig, FunSig(string_sig,string_sig))) in
    (op_type, Type_witness.MonoDyn (op_sig, op))

  let string_split =
    let op_type = poly_t 0 (fun_t string_t (fun_t string_t (Type.GeneratorType string_t))) in
    let op_sig = Type_witness.(FunSig(string_sig, FunSig(string_sig, GeneratorSig string_sig))) in
    (op_type, Type_witness.MonoDyn (op_sig, Series.RE.split))

  let string_extract =
    let op_type = poly_t 0 (fun_t string_t (fun_t string_t (Type.GeneratorType string_t))) in
    let op_sig = Type_witness.(FunSig(string_sig, FunSig(string_sig, GeneratorSig string_sig))) in
    (op_type, Type_witness.MonoDyn (op_sig, Series.RE.extract))

  let files =
    let op_type = poly_t 0 (fun_t string_t (Type.GeneratorType string_t)) in
    let op_sig = Type_witness.(FunSig(string_sig, GeneratorSig string_sig)) in
    (op_type, Type_witness.MonoDyn (op_sig, Series.IO.files))

  let file_chunks =
    let op_type = poly_t 0 (fun_t string_t (Type.GeneratorType string_t)) in
    let op_sig = Type_witness.(FunSig(string_sig, GeneratorSig string_sig)) in
    (op_type, Type_witness.MonoDyn (op_sig, Series.IO.file_chunks 1024))

  let file_split char =
    let op_type = poly_t 0 (fun_t string_t (Type.GeneratorType string_t)) in
    let op_sig = Type_witness.(FunSig(string_sig, GeneratorSig string_sig)) in
    (op_type, Type_witness.MonoDyn (op_sig, Series.IO.file_split char))

  let kyoto_pairs =
    let op_type = poly_t 0 Type.(fun_t string_t (GeneratorType (PairType (string_t,string_t)))) in
    let op_sig = Type_witness.(FunSig(string_sig, GeneratorSig (PairSig (string_sig,string_sig)))) in
    (op_type, Type_witness.MonoDyn (op_sig, KyotoCabinet.KVStore.all_pairs))

  let kyoto_pairs_with_prefix =
    let op_type = poly_t 0 Type.(fun_t string_t (fun_t string_t (GeneratorType (PairType (string_t,string_t))))) in
    let op_sig = Type_witness.(FunSig(string_sig, FunSig(string_sig, GeneratorSig (PairSig (string_sig,string_sig))))) in
    (op_type, Type_witness.MonoDyn (op_sig, KyotoCabinet.KVStore.pairs_with_prefix))

  let kyoto_pairs_within_range =
    let op_type = poly_t 0 Type.(fun_t string_t (fun_t string_t (fun_t string_t (GeneratorType (PairType (string_t,string_t)))))) in
    let op_sig = Type_witness.(FunSig(string_sig, FunSig(string_sig, FunSig(string_sig, GeneratorSig (PairSig (string_sig,string_sig)))))) in
    (op_type, Type_witness.MonoDyn (op_sig, KyotoCabinet.KVStore.pairs_within_range))

  let kyoto =
    let db_t = Type.(record_t [
      "keys", GeneratorType string_t;
      "pairs", GeneratorType (PairType(string_t,string_t));
      "values", FunType (string_t, GeneratorType string_t);
      "pairs_with_prefix", FunType (string_t, GeneratorType (PairType(string_t,string_t)));
      "pairs_within_range", FunType (string_t, FunType(string_t, GeneratorType (PairType(string_t,string_t))));
    ]) in
    let db_sig = Type_witness.(RecordSig ( RecordSig ( RecordSig ( RecordSig ( RecordSig (UnitSig,
      "keys", GeneratorSig string_sig),
      "pairs", GeneratorSig (PairSig(string_sig,string_sig))),
      "values", FunSig (string_sig, GeneratorSig string_sig)),
      "pairs_with_prefix", FunSig (string_sig, GeneratorSig (PairSig(string_sig,string_sig)))),
      "pairs_within_range", FunSig (string_sig, FunSig(string_sig, GeneratorSig (PairSig(string_sig,string_sig)))))
    ) in
    let open_val path = KyotoCabinet.KVStore.(
      pairs_within_range path, (
      pairs_with_prefix path, (
      get_values path, (
      all_pairs path, (
      all_keys path, (
    )))))) in
    let open_type = poly_t 0 Type.(fun_t string_t db_t) in
    let open_sig = Type_witness.(FunSig(string_sig, db_sig)) in
    (open_type, Type_witness.MonoDyn (open_sig, open_val))

  let col_of_result f g s = Serialization.Result.to_collection (f g s)

  let json_decoder =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (ShapeType a) (fun_t string_t (Type.GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(ShapeSig a,FunSig(string_sig, GeneratorSig a)),
      col_of_result (Serialization.format_decoder Json_support.format))
    })

  let json_encoder =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (ShapeType a) (fun_t a (Type.GeneratorType string_t)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(ShapeSig a,FunSig(a, GeneratorSig string_sig)),
      col_of_result (Serialization.format_encoder Json_support.format))
    })

  let marshal_decoder =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (ShapeType a) (fun_t string_t (Type.GeneratorType a)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(ShapeSig a,FunSig(string_sig, GeneratorSig a)),
      col_of_result (Serialization.format_decoder Marshal_support.format))
    })

  let marshal_encoder =
    let a = Type.gen_t 0 in
    let t = Type.(poly_t 1 (fun_t (ShapeType a) (fun_t a (Type.GeneratorType string_t)))) in
    Type_witness.(t, PolyDyn { make = fun a ->
      MonoDyn(FunSig(ShapeSig a,FunSig(a, GeneratorSig string_sig)),
      col_of_result (Serialization.format_encoder Marshal_support.format))
    })

  let initial_env = initial_env
    |> define "++" (string_operator (^))
    |> define "string_split" string_split
    |> define "string_extract" string_extract
    |> define "files" files
    |> define "file_chunks" file_chunks
    |> define "file_lines" (file_split '\n')
    |> define "kyoto" kyoto
    |> define "json_decode" json_decoder
    |> define "json_encode" json_encoder
    |> define "marshal_decode" marshal_decoder
    |> define "marshal_encode" marshal_encoder

  let _ = Parsing_helper.(List.iter add_operator [
    "++", LeftAssoc, 8;
  ])
end

