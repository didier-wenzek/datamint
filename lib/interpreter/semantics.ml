(*
  This module has been implemented after the paper
  "Finally Tagless, Partially Evaluated
   Tagless Staged Interpreters for Simpler Typed Languages"
  by Jacques Carette, Oleg Kiselyov and Chung-chieh Shan.
  http://okmij.org/ftp/tagless-final/JFP.pdf

  The type names have been changed:
  it helps me to understand the misc type signatures.
*)

module type S = sig
  (* Representation of a value using some concret type 'a. *)
  type 'a repr

  (* Evaluation environment, which uses a value of type 'e to store runtime values. *)
  type 'e env

  (* A compiled expression, which evaluates to a value of type 'a in an environment of type 'e *)
  type ('a, 'e) cexpr

  (* An environment of type 'e, extended to contain a value of type 'a *)
  type ('a, 'e) exenv

  (* Type of the empty environment *)
  type empty_env

  (* Evaluate the compiled expression in an environment. *)
  val eval: ('a, 'e) cexpr -> 'e env -> 'a repr

  val varZ: ('a, ('a,'b) exenv) cexpr
  val varS: ('a, 'e) cexpr -> ('a, ('b,'e) exenv) cexpr

  val lam: ('b, ('a,'e) exenv) cexpr -> ('a -> 'b,'e) cexpr
  val app: ('a -> 'b,'e) cexpr -> ('a,'e) cexpr -> ('b,'e) cexpr

  val empty_env: empty_env env
  val extent_env: 'a repr -> 'e env -> ('a, 'e) exenv env

  val unit: (unit, 'e) cexpr
  val pair: ('a,'e) cexpr -> ('b,'e) cexpr -> ('a*'b,'e) cexpr
  val fst: ('a*'b,'e) cexpr -> ('a,'e) cexpr
  val snd: ('a*'b,'e) cexpr -> ('b,'e) cexpr

  val constant: 'a -> ('a,'e) cexpr
end

exception Compilation_error of string

module TaglessFinal : S with
  type 'a repr = 'a
  and type ('a,'b) exenv = 'a * 'b
  and type ('a,'b) cexpr = ('b -> 'a)
= struct
  type 'a repr = 'a
  type 'e env = 'e
  type ('a,'b) cexpr = ('b -> 'a)
  type empty_env = unit
  type ('a,'b) exenv = 'a * 'b

  let eval e env = e env

  let varZ env = fst env
  let varS var env = var (snd env)

  let lam body env = fun x -> body (x,env)
  let app e1 e2 env = (e1 env) (e2 env)

  let empty_env = ()
  let extent_env a env = (a,env)

  let unit env = ()
  let pair a b env = (a env, b env)
  let fst p env = fst (p env)
  let snd p env = snd (p env)

  let constant a env = a
end
