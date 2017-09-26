open React
open Lwt
open LTerm_text

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

module Make(I: Interpreter) = struct

  (* +-----------------------------------------------------------------+
     | Prompt and output wrapping                                      |
     +-----------------------------------------------------------------+ *)

  let display color prompt line =
    eval [ B_bold true; B_fg color ; S prompt; E_fg; E_bold; S line ]

  let make_intro =
    display LTerm_style.blue I.intro ""

  (* Create a prompt based on the current interpreter state *)
  let make_prompt n =
    let prompt = Printf.sprintf "%s [%d]: " I.prompt n in
    display LTerm_style.blue prompt ""

  (* Format the interpreter output for REPL display *)
  let make_output n out =
    let prompt = Printf.sprintf "%s [%d]: " I. prompt_out n in
    display LTerm_style.green prompt out

  let make_error n err =
    let prompt = Printf.sprintf "%s [%d]: " I.prompt_error n in
    display LTerm_style.red prompt err

  (* +-----------------------------------------------------------------+
     | Customization of the read-line engine                           |
     +-----------------------------------------------------------------+ *)

  class read_line ~term ~history ~step = object(self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_utf8.t] LTerm_read_line.term term

    method show_box = false

    initializer
      self#set_prompt (S.const (make_prompt step));
  end

  (* +-----------------------------------------------------------------+
     | Cancelation of background tasks                                 |
     +-----------------------------------------------------------------+ *)

  let running_tasks = Lwt_sequence.create ()

  let rec cancel_running_tasks i =
    match Lwt_sequence.take_opt_l running_tasks with
    | Some t -> (Lwt.wakeup_later_exn t Lwt.Canceled; cancel_running_tasks i)
    | None -> ()

  let rec watch_control_c term =
    LTerm.read_event term >>= function
      | LTerm_event.Key k
        when LTerm_key.control k = true
        &&   LTerm_key.code k = LTerm_key.Char(CamomileLibraryDyn.Camomile.UChar.of_char 'c') ->
        cancel_running_tasks 0;
        return_unit
      | _ ->
        watch_control_c term

  let cancel_on_control_c term =
    LTerm.enter_raw_mode term
    >>= fun mode ->
    watch_control_c term
    >>= fun () -> 
    LTerm.leave_raw_mode term mode

  let run_unless_canceled term t =
    let cancel = Lwt.add_task_r running_tasks in
    (*Lwt.async (fun () -> Lwt.pick [cancel_on_control_c term; cancel]);*)
    Lwt.pick [t; cancel] 

  (* +-----------------------------------------------------------------+
     | Parsing and evaluation                                          |
     +-----------------------------------------------------------------+ *)

  let parse buffer line =
     Buffer.add_string buffer line;
     let cmd = Buffer.contents buffer in
     let expr = I.parse cmd in
     ( match expr with
       | UncompleteExpr -> Buffer.add_char buffer '\n'
       | _ -> Buffer.clear buffer
     );
     return (cmd,expr)

  let eval state expr =
    Lwt.catch
      (fun () -> I.eval state expr)
      (fun exn -> return (state, EvalError exn))

  let display_result term step = function
    | ResultValue value -> LTerm.fprintls term (make_output step value)
    | ResultPrinter print -> (
      Lwt.catch
        (fun () -> run_unless_canceled term (print (LTerm.fprint term)))
        (function
          | Lwt.Canceled -> LTerm.fprintls term (make_error step "Display interrupted")
          | exn -> LTerm.fprintls term (make_error step (I.error_msg exn))
        )
    )
    | EvalError exn -> LTerm.fprintls term (make_error step (I.error_msg exn))

  (* +-----------------------------------------------------------------+
     | Main loop                                                       |
     +-----------------------------------------------------------------+ *)

  let rec read_line term history step =
    Lwt.catch
      (fun () ->
        let history = LTerm_history.contents history in
        let rl = new read_line ~term ~history ~step in
        rl#run)
      (function
        | Sys.Break ->
          LTerm.fprintl term "Interrupted"
          >>= fun () ->
          read_line term history step
        | exn -> Lwt.fail exn)

  let rec loop term history history_file step buffer state =
    read_line term history step
    >>= fun line ->
    parse buffer line
    >>= function
    | command, ParsedExpr expr ->
      eval state expr
      >>= fun (state, result) ->
      display_result term step result
      >>= fun () ->
      LTerm_history.add history command;
      LTerm_history.save history history_file
      >>= fun () ->
      loop term history history_file (step+1) buffer state
    | command, ParseError exn ->
      LTerm.fprintls term (make_error step (I.error_msg exn))
      >>= fun () ->
      LTerm_history.add history command;
      loop term history history_file step buffer state
    | _, UncompleteExpr ->
      loop term history history_file step buffer state

  (* +-----------------------------------------------------------------+
     | Entry point                                                     |
     +-----------------------------------------------------------------+ *)

  let home file =
    try String.(
      if index file '~' = 0
      then (Sys.getenv "HOME") ^ (sub file 1 ((length file) - 1))
      else file
    )
    with Not_found -> file

  let main config =
    let history_file = home I.history_file in
    let _ = Lwt_unix.on_signal Sys.sigint cancel_running_tasks in
    LTerm_inputrc.load ()
    >>= fun () ->
    Lwt.catch (fun () ->
      I.init config
      >>= fun state ->
      Lazy.force LTerm.stdout
      >>= fun term ->
      LTerm.fprintls term make_intro
      >>= fun () ->
      LTerm_history.create []
      |> fun history ->
      LTerm_history.load history history_file
      >>= fun () ->
      let buffer = Buffer.create 256 in
      let step = 1 in
      loop term history history_file step buffer state)
      (function
        | LTerm_read_line.Interrupt -> Lwt.return ()
        | exn -> Lwt.fail exn)
end
