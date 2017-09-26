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
  | Dot of 'def * string         (* expr.f is transformed into App(Dot("_","f"),expr) *)
  | Tag of 'def * string * ('var,'def) expr (* A(expr) is transformed into Tag("_","A",expr) *)
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

(* Transform an untyped expression
   into an expression where each variable is introduced with its type.

   Return the transformed expression along its type.
*)
val infer_types: Type.Env.env -> untyped_expr -> (typed_expr * Type.polytyp)
val typed_expr: Type.Env.env -> untyped_expr -> typed_expr
val typeof: Type.Env.env -> untyped_expr -> Type.polytyp

(* Transform an expression where the variables are denoted by name
   into an expression where variables are denoted using De Bruijn indices.

   The transformed expression is returned along the list of free variables.
*)
val de_bruijn_expr: untyped_expr -> (de_bruijn_expr * name list)

(* Transform a typed expression in an expression using De Bruijn indices.

   Each usage of a free polymorphic variable (say id[Int] and id[String])
   leads to a specific free variable.

   `App (App (Var id[Int->Int]) (Var id[Int))) (Int 13)`

   is transformed into:

   `App (App (Var VZ) (Var (VS VZ))) (Int 13)` with the free vars `|id[Int->Int]|id[Int)|`
*)
val de_bruijn_typed: typed_expr -> (typed_de_bruijn_expr * (name * (Type.typ list)) list)

val rename_variables: (DeBruijn.t -> DeBruijn.t) -> (DeBruijn.t, 'a) expr -> (DeBruijn.t, 'a) expr

val show_expr: ('a -> string) -> ('b -> string) -> ('a,'b) expr -> string
val show_untyped: untyped_expr -> string
val show_typed: typed_expr -> string
val show_de_bruijn: de_bruijn_expr -> string
val show_de_bruijn_typed: typed_de_bruijn_expr -> string

val show_env_types: (name * (Type.typ list)) list -> string
