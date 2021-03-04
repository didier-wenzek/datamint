open Lwt
open DataMint.Interpreter
module Process = DataMint.Series.Process

module Runtime = Core_functional_language.Make(Semantics_witness.TaglessFinal)

let stdlib =
  let open Filename in
  concat (dirname Sys.executable_name) "stdlib.sift"

module Interpreter = struct
  module Pars = Parsing_helper

  type config = unit
  type state = Runtime.env
  type expr = (string option * Expr.untyped_expr) list

  let intro        = "Core Functional Language"
  let prompt       = ">>> "
  let prompt_out   = "    "
  let prompt_error = "err "
  let history_file = "~/.cfl_history"

  let init () =
    Runtime.initial_env
    |> Runtime.load_file stdlib
    |> return

  let parse str =
    try Lambda_term_repl.ParsedExpr (Runtime.parse_string str)
    with
    | Pars.UnexpectedEof -> Lambda_term_repl.UncompleteExpr
    | exp -> Lambda_term_repl.ParseError exp

  let eval env exprs =
    try
      let env, values = Runtime.eval_exprs env exprs in
      return (env, Lambda_term_repl.ResultPrinter (Runtime.display values))
    with exn ->
      return (env, Lambda_term_repl.EvalError exn)

  let error_msg = Runtime.error_msg
end

module EvalLoop = Lambda_term_repl.Make(Interpreter)

module BgInterpreter = struct

  let main files =
    let loop () =
      Runtime.initial_env
      |> Runtime.load_file stdlib
      |> Runtime.load_files files
      |> ignore (* env *)
      |> Process.wait_processes
    in
    let print_error e =
      Lwt_io.eprintf "%s\n" (Runtime.error_msg e)
    in
    Lwt.catch loop print_error
end

let main () =
  let argv = Array.to_list Sys.argv in
  let files = match argv with _::files -> files | _ -> [] in
  let repl = match files with [] -> true | _ -> false in
  Lwt_main.run (
    if repl 
    then EvalLoop.main ()
    else BgInterpreter.main files
  )

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  main ()
