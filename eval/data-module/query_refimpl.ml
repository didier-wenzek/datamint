open Util

exception Unbounded
exception Unboundable

module Var : sig
  type ('a,'context) var
  (** A variable of type ['a] in a context of type ['context].

      In practice, the ['context] is a product type
      with one component being of type ['a].

      The idea is to leverage the type system
      to ensure that variables and relations are correctly chained in a query.
      
      let post, auth, uuid, title, date = var5 () in
      select (!$ uuid $ title $ date) [
        !! post Post.author auth;
        !! auth Author.name (value name);
        !! post Post.uuid uuid;
        !! post Post.title title;
        !! post Post.date date;
      ] *)

  type ('stack,'context) stack_layout
  (** Describe the layout of a stack of type ['stack]
      built in a context of type ['context].

      Here, both ['stack] and ['context'] are product types,
      ['stack] being a permutation of some of ['context] components. *)

  type ('a,'context) selection
  (** Describe a value of type ['a] built from a context of type ['context].

      Here, both ['a] and ['context'] are product types,
      ['a] being a permutation of some of ['context] components. *)

  val var1 : unit ->
           ('a, 'a * unit) var
  (** A context with a single variable. *)

  val var2 : unit ->
           ('a, 'a * ('b * unit)) var *
           ('b, 'a * ('b * unit)) var
  (** A context with 2 variables. *)

  val var3 : unit ->
           ('a, 'a * ('b * ('c * unit))) var *
           ('b, 'a * ('b * ('c * unit))) var *
           ('c, 'a * ('b * ('c * unit))) var
  (** A context with 3 variables. *)

  val var4 : unit ->
           ('a, 'a * ('b * ('c * ('d * unit)))) var *
           ('b, 'a * ('b * ('c * ('d * unit)))) var *
           ('c, 'a * ('b * ('c * ('d * unit)))) var *
           ('d, 'a * ('b * ('c * ('d * unit)))) var
  (** A context with 4 variables. *)

  val var5 : unit ->
           ('a, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('b, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('c, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('d, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('e, 'a * ('b * ('c * ('d * ('e * unit))))) var
  (** A context with 5 variables. *)

  val __ : ('a,'b) var
  (** A variable which value is to be ignored. *)

  val value: 'a -> ('a,'b) var
  (** A variable which value is given. *)

  val extractor : ('a,'c) var -> ('b,'c) stack_layout -> ('b -> 'a) option
  val empty_stack: ('a,'c) var -> (unit, 'c) stack_layout
  val push : ('a,'c) var -> ('b,'c) stack_layout -> ('a * 'b, 'c) stack_layout

  val all: ('a,'a) selection
  (** Select all the variables of the context,
      in the declaration order. *)

  val (!$): ('a,'b) var -> ('a,'b) selection
  (** Select only the given variable. *)

  val ($): ('a,'c) selection -> ('b,'c) var -> ('a*'b, 'c) selection
  (** Add the given variable to the selection. *)

end
= struct

  type ('stack,'context) stack_layout =
    | NilCtx : ('stack, unit) stack_layout
    | ConsCtx : (('stack -> 'a) option * ('stack,'context) stack_layout) -> ('stack, 'a * 'context) stack_layout
  (** An heterogeneous list
      which gives the stack extractor associated to each variable of the context. *)

  type ('a,'context) var_hdl = {
    extractor: 'stack. ('stack,'context) stack_layout -> ('stack -> 'a) option;
    injector: 'stack. ('stack,'context) stack_layout -> ('a * 'stack,'context) stack_layout; 
    empty_layout: (unit, 'context) stack_layout;
  }

  type ('a,'b) var =
    | Ignore
    | Val of 'a
    | Var of ('a,'b) var_hdl

  type ('a,'context) selection = {
    project: 'stack. ('stack,'context) stack_layout -> 'stack -> 'a;
  }

  let var x = Var x

  let __ = Ignore

  let value x = Val x

  let shift_extractor = function
    | None -> None
    | Some extr -> Some (fun (h,t) -> extr t)

  let rec extend: type a. ('b,a) stack_layout -> ('c * 'b, a) stack_layout = function
    | ConsCtx (extr, others) -> ConsCtx (shift_extractor extr, extend others)
    | NilCtx -> NilCtx

  let here empty_layout = {
    extractor = (function ConsCtx (ext, _) -> ext);
    injector = (function ConsCtx (_, others) -> ConsCtx (Some fst, extend others));
    empty_layout;
  } 

  let next x = {
    extractor = (function ConsCtx (_, others) -> x.extractor others);
    injector = (function
      | ConsCtx(extr, others) -> ConsCtx (shift_extractor extr, x.injector others)
    );
    empty_layout = ConsCtx(None,x.empty_layout);
  }

  let extractor = function
    | Ignore -> (fun _ -> None)
    | Val x -> (fun _ -> Some (fun _ -> x))
    | Var var -> var.extractor

  let empty_stack = function
    | Var var -> var.empty_layout
    | _ -> raise Unboundable

  let push = function
    | Var var -> var.injector
    | _ -> raise Unboundable

  let (!$) = function
    | Var var -> { project = fun st ->
      match var.extractor st with
      | Some extr -> extr
      | None -> raise Unbounded
    }
    | Val x -> { project = fun _ _ -> x }
    | Ignore -> { project = fun _ -> raise Unbounded }

  let ($) base = function
    | Var var -> { project = fun st ->
      match var.extractor st with
      | Some tail_extr ->
        let head_extr = base.project st in
        fun s -> (head_extr s, tail_extr s)
      | None -> raise Unbounded
    }
    | Val x -> { project = fun st -> 
      let head_extr = base.project st in
      fun s -> (head_extr s, x)
    }
    | Ignore -> { project = fun _ -> raise Unbounded }

  let rec extract_all: type a. ('b,a) stack_layout -> 'b -> a = function
    | NilCtx -> (fun s -> ())
    | ConsCtx (None, _) -> raise Unbounded
    | ConsCtx (Some head_extr, others) ->
      let tail_extr = extract_all others in
      fun s -> (head_extr s, tail_extr s)

  let all = { project = extract_all }

  let nil = NilCtx
  let undef tail = ConsCtx (None, tail)

  let var1 () =
    let x1 = here (undef nil) in
    (var x1)

  let var2 () =
    let x1 = here (undef (undef nil)) in
    let x2 = next (here  (undef nil)) in
    (var x1, var x2)

  let var3 () =
    let x1 = here (undef (undef (undef nil))) in
    let x2 = next (here  (undef (undef nil))) in
    let x3 = next (next  (here  (undef nil))) in
    (var x1, var x2, var x3)

  let var4 () =
    let x1 = here (undef (undef (undef (undef nil)))) in
    let x2 = next (here  (undef (undef (undef nil)))) in
    let x3 = next (next  (here  (undef (undef nil)))) in
    let x4 = next (next  (next  (here  (undef nil)))) in
    (var x1, var x2, var x3, var x4)

  let var5 () =
    let x1 = here (undef (undef (undef (undef (undef nil))))) in
    let x2 = next (here  (undef (undef (undef (undef nil))))) in
    let x3 = next (next  (here  (undef (undef (undef nil))))) in
    let x4 = next (next  (next  (here  (undef (undef nil))))) in
    let x5 = next (next  (next  (next  (here  (undef nil))))) in
    (var x1, var x2, var x3, var x4, var x5)
end

module Make(Schema: Schema.S): Query.S
  with type ('a,'b) relation = ('a,'b) Schema.relation
  and  type 'a collection = 'a Schema.collection
  and  type 'a result = 'a Schema.value
= struct
  include Var

  type ('a,'b) relation = ('a,'b) Schema.relation
  type 'a collection = 'a Schema.collection
  type 'a result = 'a Schema.value

  type 'a clause =
    | Clause: (('a,'c) var * ('a,'b) relation * ('b,'c) var) -> 'c clause

  type 'a query =
    | Query: ('a,'b) selection * 'b clause list -> 'a query

  let select selection query = Query (selection, query)

  let (!!) x rel y = Clause (x,rel,y)

  let run q = raise (Invalid_argument "Not implemented")
end

