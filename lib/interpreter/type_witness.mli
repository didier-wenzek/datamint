module Bounded = Series.Bounded
module Reducer = Series.Reducer

type 'a impl = {
  to_string: 'a -> string;
  of_string: string -> 'a;
  repr: 'a Generics.Repr.t;
}

(* Phantom type without values, used to compile expression with unused type variables. *)
type void

type _ t =
  | UnitSig: unit t
  | NativeSig: string * 'a impl -> 'a t
  | FunSig: 'a t * 'b t -> ('a -> 'b) t
  | PairSig: 'a t * 'b t -> ('a * 'b) t
  | RecordSig: 'a t * string * 'b t -> ('b * 'a) t
  | GeneratorSig: 'a t -> 'a Bounded.producer t
  | ReducerSig: 'a t * 'b t * 'c t -> ('a,'b,'c) Reducer.t t
  | ShapeSig: 'a t -> 'a Generics.Repr.t t
  | VoidSig: void t

type type_sig =
  | Sig: 'a t -> type_sig

type dyn =
  | Dyn: 'a t * 'a -> dyn

type poly_dyn =
  | MonoDyn: 'a t * 'a -> poly_dyn
  | PolyDyn: { make: 'a. 'a t -> poly_dyn } -> poly_dyn

type _ field_getter =
  FieldGetter: ('a -> 'b) * 'b t -> 'a field_getter

type (_,_) eq = Eq : ('a,'a) eq
val eq_type: 'a t -> 'b t -> ('a,'b) eq option

val unit: unit t
val lam: 'a t -> 'b t -> ('a -> 'b) t
val pair: 'a t -> 'b t -> ('a *'b) t
val record: 'a t -> string -> 'b t -> ('b *'a) t
val field_getter: string -> 'a t  -> 'a field_getter

val add_impl: string -> type_sig -> unit

val dyn_of_literal: Type.typ -> string -> dyn
val dyn_of_shape: Datatype.t -> dyn
val gen_val_of_sig: 'a t -> 'a Generics.Repr.t option

val eval_type: type_sig list -> Type.typ -> type_sig
val specialize_polydyn: type_sig list -> (poly_dyn * Type.typ list) -> dyn

val show: 'a t -> string
val show_poly_dyn: Type.polytyp -> poly_dyn -> string*string
