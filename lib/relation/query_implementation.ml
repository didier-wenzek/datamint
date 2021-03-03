open Series.Util

exception Unbounded
exception Unboundable
exception NoPlan

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
  val push_var: ('a,'c) var -> ('b,'c) stack_layout -> ('a * 'b, 'c) stack_layout
  val push_val: 'a -> 'b -> ('b * 'a)

  val all: ('a,'a) selection
  (** Select all the variables of the context,
      in the declaration order. *)

  val (!$): ('a,'b) var -> ('a,'b) selection
  (** Select only the given variable. *)

  val ($): ('a,'c) selection -> ('b,'c) var -> ('a*'b, 'c) selection
  (** Add the given variable to the selection. *)

  val selection_extractor: ('a, 'context) selection -> ('b, 'context) stack_layout -> ('b -> 'a)
  (** [selection_extractor selection stack_layout]
      returns the function to extract the selection of the stack. *)
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
    | Some extr -> Some (fun (_h,t) -> extr t)

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

  let push_var = function
    | Var var -> var.injector
    | _ -> raise Unboundable

  let push_val s x = (x,s)

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

  let selection_extractor selection stack_layout =
    selection.project stack_layout

  let rec extract_all: type a. ('b,a) stack_layout -> 'b -> a = function
    | NilCtx -> (fun _ -> ())
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

module Make(Schema: Interpretation.S): Query.S
= struct
  include Var
  include Schema

  type 'context clause =
    | Clause: (('a,'context) var * ('a,'b,'ab_gen,'a_gen,'b_gen) relation * ('b,'context) var) -> 'context clause

  type 'a query =
    | Query: ('a,'context) selection * 'context clause list -> 'a query

  type 'context plan =
    | Plan: ('stack, 'context) stack_layout * (unit records -> 'stack records) -> 'context plan

  let select selection query = Query (selection, query)

  let (!!) x rel y = Clause (x,rel,y)

  (* Naive compilation:
     it peeks the first clause which can be applied,
     looping until there is no more clause.

     It raises the NoPlan exception if stuck. *)
  let compile =
    let then_filter (Plan (stack_layout, query)) (Clause (x,rel,y)) =
      match Schema.filter rel, extractor x stack_layout, extractor y stack_layout with
      | filter, Some x_extr, Some y_extr ->
        Some (Plan(stack_layout, query >> filter x_extr y_extr))
      | _ -> None
    in

    let then_map (Plan (stack_layout, query)) (Clause (x,rel,y)) =
      match Capability.get_opt (Schema.map_cap rel), extractor x stack_layout, extractor y stack_layout with
      | Some map, Some x_extr, None ->
        Some (Plan(push_var y stack_layout, query >> map x_extr push_val))
      | _ -> None
    in

    let then_inv (Plan (stack_layout, query)) (Clause (x,rel,y)) =
      match Capability.get_opt (Schema.inv_cap rel), extractor x stack_layout, extractor y stack_layout with
      | Some inv, None, Some y_extr ->
        Some (Plan(push_var x stack_layout, query >> inv push_val y_extr))
      | _ -> None
    in

    let then_gen (Plan (stack_layout, query)) (Clause (x,rel,y)) =
      match Capability.get_opt (Schema.gen_cap rel), extractor x stack_layout, extractor y stack_layout with
      | Some gen, None, None ->
        Some (Plan(stack_layout |> push_var x |> push_var y, query >> gen push_val push_val))
      | _ -> None
    in

    let then_apply op plan =
      let rec loop discarded = function
        | [] -> raise NoPlan             (* FIXME *)
        | clause :: others -> (
          match op plan clause with
          | None -> loop (clause::discarded) others
          | Some updated_plan -> (updated_plan, List.rev_append discarded others)
        )
      in loop []
    in

    let then_apply_in_turn ops plan clauses =
      let rec loop = function
        | [] -> raise NoPlan             (* FIXME *)
        | op :: others -> (
          try then_apply op plan clauses
          with NoPlan -> loop others
        )
      in
      loop ops
    in

    let rec loop plan = function
      | [] -> plan
      | clauses ->
        let updated_plan, remaining_clauses =
          then_apply_in_turn [then_filter; then_map; then_inv; then_gen] plan clauses
        in
        loop updated_plan remaining_clauses
    in

    let init_plan =
      let stack_of_var x =
        try Some (empty_stack x)
        with Unboundable -> None
      in
      let stack_of_clause (Clause (x,_,y)) =
        match stack_of_var x with
        | None -> stack_of_var y
        | s -> s
      in
      let rec stack_of_clauses = function
        | [] -> raise NoPlan             (* FIXME *) 
        | c :: cs ->
          match stack_of_clause c with
          | None -> stack_of_clauses cs
          | Some s -> s
      in
      fun clauses ->
        Plan (stack_of_clauses clauses, id)
    in

    fun clauses ->
      loop (init_plan clauses) clauses

  let run (Query (selection, clauses)) =
    let Plan(stack_layout,query) = compile clauses in
    let projection = selection_extractor selection stack_layout in
    Schema.record_source |> query |> Schema.map projection
end
