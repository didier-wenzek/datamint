open Util

exception Unbounded
exception Unboundable

module Var : sig
  type ('a,'b) var
  (** A variable of type ['a] in a context of type ['b].

      In practice, ['b] is a product type
      with one component being ['a].

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

  type ('a,'b) stack
  (** A stack of type ['a] in a context of type ['b].

      Here, both ['a] and ['b'] are product types,
      ['a] being a permutation of some of ['b] components. *)

  val var1 : unit ->
           ('a, 'a * unit) var

  val var2 : unit ->
           ('a, 'a * ('b * unit)) var *
           ('b, 'a * ('b * unit)) var

  val var3 : unit ->
           ('a, 'a * ('b * ('c * unit))) var *
           ('b, 'a * ('b * ('c * unit))) var *
           ('c, 'a * ('b * ('c * unit))) var

  val var4 : unit ->
           ('a, 'a * ('b * ('c * ('d * unit)))) var *
           ('b, 'a * ('b * ('c * ('d * unit)))) var *
           ('c, 'a * ('b * ('c * ('d * unit)))) var *
           ('d, 'a * ('b * ('c * ('d * unit)))) var

  val var5 : unit ->
           ('a, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('b, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('c, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('d, 'a * ('b * ('c * ('d * ('e * unit))))) var *
           ('e, 'a * ('b * ('c * ('d * ('e * unit))))) var

  val __ : ('a,'b) var
  val value: 'a -> ('a,'b) var

  val extractor : ('a,'c) var -> ('b,'c) stack -> ('b -> 'a) option
  val empty_stack: ('a,'c) var -> (unit, 'c) stack
  val push : ('a,'c) var -> ('b,'c) stack -> ('a * 'b, 'c) stack

  type ('a,'b) selection
  val all: ('a,'a) selection
  val (!$): ('a,'b) var -> ('a,'b) selection
  val ($): ('a,'c) selection -> ('b,'c) var -> ('a*'b, 'c) selection

end
= struct

  type ('a,'b) hdl =
    | NilCtx : (unit, 'c) hdl
    | ConsCtx : (('c -> 'a) option * ('b,'c) hdl) -> ('a * 'b, 'c) hdl

  type 'a ctx = ('a, unit) hdl

  type ('a,'b) var_hdl = {
    extractor: 'c. ('b,'c) hdl -> ('c -> 'a) option;
    injector: 'c. ('b,'c) hdl -> ('b,'a * 'c) hdl; 
  }

  type ('a,'b) t = {
    hdl: ('a,'b) var_hdl;
    context: 'b ctx;
  }

  type ('a,'b) stack = ('b,'a) hdl

  type ('a,'b) var =
    | Ignore
    | Val of 'a
    | Var of ('a,'b) t

  type ('a,'b) selection = {
    project: 'c. ('b,'c) hdl -> 'c -> 'a;
  }

  let var x ctx = Var {
    hdl = x;
    context = ctx;
  }

  let __ = Ignore

  let value x = Val x

  let shift_extractor = function
    | None -> None
    | Some extr -> Some (fun (h,t) -> extr t)

  let rec extend: type a. (a,'b) hdl -> (a, 'c * 'b) hdl = function
    | ConsCtx (extr, others) -> ConsCtx (shift_extractor extr, extend others)
    | NilCtx -> NilCtx

  let here = {
    extractor = (function ConsCtx (ext, _) -> ext);
    injector = (function ConsCtx (_, others) -> ConsCtx (Some fst, extend others));
  } 

  let next f = {
    extractor = (function ConsCtx (_, others) -> f.extractor others);
    injector = (function
      | ConsCtx(extr, others) -> ConsCtx (shift_extractor extr, f.injector others)
    );
  }

  let extractor = function
    | Ignore -> (fun _ -> None)
    | Val x -> (fun _ -> Some (fun _ -> x))
    | Var var -> var.hdl.extractor

  let empty_stack = function
    | Var var -> var.context
    | _ -> raise Unboundable

  let push = function
    | Var var -> var.hdl.injector
    | _ -> raise Unboundable

  let (!$) = function
    | Var var -> { project = fun st ->
      match var.hdl.extractor st with
      | Some extr -> extr
      | None -> raise Unbounded
    }
    | Val x -> { project = fun _ _ -> x }
    | Ignore -> { project = fun _ -> raise Unbounded }

  let ($) base = function
    | Var var -> { project = fun st ->
      match var.hdl.extractor st with
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

  let rec extract_all: type a. (a,'b) hdl -> 'b -> a = function
    | NilCtx -> (fun s -> ())
    | ConsCtx (None, _) -> raise Unbounded
    | ConsCtx (Some head_extr, others) ->
      let tail_extr = extract_all others in
      fun s -> (head_extr s, tail_extr s)

  let all = { project = extract_all }

  let nil = NilCtx
  let undef tail = ConsCtx (None, tail)

  let var1 () =
    let x1 = here in
    let ctx = undef nil in
    (var x1 ctx)

  let var2 () =
    let x1 = here in
    let x2 = next here in
    let ctx = undef (undef nil) in
    (var x1 ctx, var x2 ctx)

  let var3 () =
    let x1 = here in
    let x2 = next here in
    let x3 = next (next here) in
    let ctx = undef (undef (undef nil)) in
    (var x1 ctx, var x2 ctx, var x3 ctx)

  let var4 () =
    let x1 = here in
    let x2 = next here in
    let x3 = next (next here) in
    let x4 = next (next (next here)) in
    let ctx = undef (undef (undef (undef nil))) in
    (var x1 ctx, var x2 ctx, var x3 ctx, var x4 ctx)

  let var5 () =
    let x1 = here in
    let x2 = next here in
    let x3 = next (next here) in
    let x4 = next (next (next here)) in
    let x5 = next (next (next (next here))) in
    let ctx = undef (undef (undef (undef (undef nil)))) in
    (var x1 ctx, var x2 ctx, var x3 ctx, var x4 ctx, var x5 ctx)
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

