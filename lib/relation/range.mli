(** The type of range definition. *)
type 'a t =
  | Empty
  | Full
  | EQ of 'a
  | NE of 'a
  | LT of 'a
  | LE of 'a
  | GT of 'a
  | GE of 'a

(** The type of range expression.

   Define a set of values as a combination of range constraints *)
type 'a expr =
  | In of 'a t
  | And of 'a expr * 'a expr
  | Or of 'a expr * 'a expr
  | Not of 'a expr

(** Returns an equivalent expression in disjonctive normal form
   without negation nor not-equal range. *)
val dnf_expr: 'a expr -> 'a expr

(** Build a module for range expressions using a specific order *)
module Make(Elt: Map.OrderedType) : sig

  type elt = Elt.t

  val eq: elt -> elt -> bool
  val ne: elt -> elt -> bool
  val lt: elt -> elt -> bool
  val le: elt -> elt -> bool
  val gt: elt -> elt -> bool
  val ge: elt -> elt -> bool

  val contain: elt -> elt t -> bool
  val overlap: elt t -> elt t -> bool

  module Expr : sig
    type nf

    val nf: elt expr -> nf
  end

end
