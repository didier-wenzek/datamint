let sprintf = Format.sprintf

module Make(Sem: Semantics_witness.S with type 'a repr = 'a) = struct (* FIXME : remove the type constraint *)
  module Pars = Parsing_helper
  include
    Collection_support.Add(
    String_support.Add(
    Integer_support.Add(
    Bool_support.Add(
    Compiler.Make(Sem)
    ))))

  let parse lexbuf =
    try Parser.main Lexer.token lexbuf
    with
    | Parsing.Parse_error -> Lexing.(
      let curr = lexbuf.lex_curr_p in
      let line = curr.pos_lnum in
      let cnum = curr.pos_cnum - curr.pos_bol in
      let token = lexeme lexbuf in
      raise (Pars.SyntaxError (line,cnum,token))
    )

  let parse_string str =
    parse (Lexing.from_string str)

  let parse_file path =
    let file = open_in path in
    try
      let exprs = parse (Lexing.from_channel file) in
      close_in_noerr file; exprs
    with e ->
      close_in_noerr file; raise e

  let eval_exprs_bg =
    let eval_bg env (oname,expr) =
      let ctype, cexpr = compile env expr in
      match oname with
      | None -> env
      | Some name -> define name (ctype, cexpr) env
    in
    List.fold_left eval_bg

  let eval_exprs =
    let loop (env,values) (oname,expr) =
      let ctype, cexpr = compile env expr in
      let env = match oname with
        | None -> env
        | Some name -> define name (ctype, cexpr) env
      in
      (env, (oname,ctype,cexpr)::values)
    in
    fun env exprs ->
      let env, values = List.fold_left loop (env,[]) exprs in
      (env, List.rev values)

  let display_value p (oname, ctype, cexpr) =
    let kind, result = show_typed_value ctype cexpr in
    let prefix = match oname with
      | None  -> ""
      | Some name -> sprintf "%s %s " name kind
    in
    Lwt.(
      p prefix
      >>= fun () ->
      p result
      >>= fun () ->
      p "\n"
    )

  let display values printer =
    Lwt_list.iter_s (display_value printer) values

  let load_file path env =
    let exprs = parse_file path in
    eval_exprs_bg env exprs

  let load_files paths env =
   List.fold_left (fun e path -> load_file path e) env paths

  let error_msg = function
    | Pars.SyntaxError (line, pos, "")                      -> sprintf "SYNTAX ERROR: line %d, pos %d, missing token" line pos
    | Pars.SyntaxError (line, pos, token)                   -> sprintf "SYNTAX ERROR: line %d, pos %d, unexpected token '%s'" line pos token
    | Expr.Unknown_value name                               -> sprintf "ERROR: unknown value '%s'" name
    | Semantics.Compilation_error err                       -> sprintf "COMPILATION ERROR: %s" err
    | Type.Unknown_type name                                -> sprintf "TYPE ERROR: unknown type '%s'" name
    | Type.Type_error (t1,t2)                               -> sprintf "TYPE ERROR: incompatible types '%s' and '%s'" (Type.show t1) (Type.show t2)
    | Type.Missing_fields record                            -> sprintf "TYPE ERROR: missing fields %s" (Type.show record)
    | Invalid_argument err                                  -> sprintf "ERROR: invalid argument: %s" err
    | exp                                                   -> sprintf "ERROR: %s\n%!" (Printexc.to_string exp)
end
