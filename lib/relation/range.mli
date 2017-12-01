(** The type of half range definitions. *)
type 'a t =
  | Empty
  | Full
  | EQ of 'a
  | NE of 'a
  | LT of 'a
  | LE of 'a
  | GT of 'a
  | GE of 'a

(** The type of range expressions.

    Define a set of values as a combination of range constraints *)
type 'a expr =
  | In of 'a t
  | And of 'a expr * 'a expr
  | Or of 'a expr * 'a expr
  | Not of 'a expr

type 'a lower_bound =
  | NoLB
  | SomeGT of 'a
  | SomeGE of 'a

type 'a upper_bound =
  | NoUB
  | SomeLT of 'a
  | SomeLE of 'a

(** Returns an equivalent expression in disjonctive normal form
    without negation nor not-equal range. *)
val dnf_expr: 'a expr -> 'a expr

(** Transforms the expression into a list of list of half-ranges.

    The outer list is a list of disjonctions (the items are combined with or).
    The inner lists are lists of conjonctions (the items are combined with and).

    An empty outer list is to be understood as the empty set.
    An empty inner list is to be understood as the full set. *)
val dnf: 'a expr -> 'a t list list

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

  (** Returns the sorted minimal disjonctive normal form of the expression.

      The result is minimal:
      - all conjonctions are either a range or an half-ranges,
      - no conjonction overlap.
      The result is sorted:
      - the conjonction ranges are sorted in increasing bounds order. *)
  val smdnf: elt expr -> (elt lower_bound * elt upper_bound) list

end
