type 'expr parse_result =
  | UncompleteExpr
  | ParsedExpr of 'expr
  | ParseError of exn

type eval_result =
  | ResultValue of string
  | ResultPrinter of ((string -> unit Lwt.t) -> unit Lwt.t)
  | EvalError of exn

module type Interpreter = sig
  type config
  type state
  type expr

  val intro: string
  val prompt: string
  val prompt_out: string
  val prompt_error: string
  val history_file: string
  val init: config -> state Lwt.t
  val parse: string -> expr parse_result
  val eval: state -> expr -> (state * eval_result) Lwt.t
  val error_msg: exn -> string
end

module Make(I: Interpreter) : sig
  val main: I.config -> unit Lwt.t
end
