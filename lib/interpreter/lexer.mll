{
  open Parser
  module Pars =  Parsing_helper
}

let var = ['a'-'z' '_'] ['a'-'z''A'-'Z''0'-'9''_']*
let idx = '$' ['0'-'9']+
let sym = ['A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_']*
let num = ['0'-'9']+ ('.'['0'-'9']+)? ('e'['+''-']?['0'-'9']+)?
let str = '"'([^'"']|"\\\"")*'"'
let op = var | ['+' '*' '/' '%' '-' '^' '~' '@' '?' '!' '>' '<' '=' '|' '&' '@' '.' ':']+
let space = ([' ' '\t'] | "\\\n") +
let comment = "#" [^'\n']* '\n'
let eof_comment = "#" [^'\n']* eof

rule token = parse
  | "!infix"        { INFIX_CMD }
  | "!infix_right"  { INFIXR_CMD }
  | "!infix_left"   { INFIXL_CMD }

  | space           { token lexbuf }
  | '\n'            { LNSEP }
  | comment         { LNSEP }

  | sym as c        { SYM(c) }
  | str as s        { STR(s) }
  | num as s        { NUM(s) }
  | idx as x        { VAR(x) }

  | '|'             { BAR }
  | '@'             { ALL }
  | ','             { COMMA }
  | '.'             { DOT }

  | "->"            { ARROW }
  | "<-"            { REV_ARROW }
  | ':'             { COLON }
  | '='             { EQ }

  | '('             { L_PAR }
  | ')'             { R_PAR }
  | '{'             { L_CURL }
  | '}'             { R_CURL }
  | '['             { L_BRACKET }
  | ']'             { R_BRACKET }

  | "with"          { WITH }
  | "in"            { REV_ARROW }
  | "seq"           { SEQ }
  | "case"          { CASE }
  | "cases"         { CASES }
  | "of"            { OF }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }

  | '-'             { MINUS }
  | op as s         { match Pars.get_operator s with
                      | None -> VAR(s)
                      | Some(op) -> (
                        match fst op, snd op with
                        | Pars.LeftAssoc,0 -> LAOP0(s)
                        | Pars.LeftAssoc,1 -> LAOP1(s)
                        | Pars.LeftAssoc,2 -> LAOP2(s)
                        | Pars.LeftAssoc,3 -> LAOP3(s)
                        | Pars.LeftAssoc,4 -> LAOP4(s)
                        | Pars.LeftAssoc,5 -> LAOP5(s)
                        | Pars.LeftAssoc,6 -> LAOP6(s)
                        | Pars.LeftAssoc,7 -> LAOP7(s)
                        | Pars.LeftAssoc,8 -> LAOP8(s)
                        | Pars.LeftAssoc,_ -> LAOP9(s)

                        | Pars.RightAssoc,0 -> RAOP0(s)
                        | Pars.RightAssoc,1 -> RAOP1(s)
                        | Pars.RightAssoc,2 -> RAOP2(s)
                        | Pars.RightAssoc,3 -> RAOP3(s)
                        | Pars.RightAssoc,4 -> RAOP4(s)
                        | Pars.RightAssoc,5 -> RAOP5(s)
                        | Pars.RightAssoc,6 -> RAOP6(s)
                        | Pars.RightAssoc,7 -> RAOP7(s)
                        | Pars.RightAssoc,8 -> RAOP8(s)
                        | Pars.RightAssoc,_ -> RAOP9(s)

                        | Pars.NonAssoc,0 -> NAOP0(s)
                        | Pars.NonAssoc,1 -> NAOP1(s)
                        | Pars.NonAssoc,2 -> NAOP2(s)
                        | Pars.NonAssoc,3 -> NAOP3(s)
                        | Pars.NonAssoc,4 -> NAOP4(s)
                        | Pars.NonAssoc,5 -> NAOP5(s)
                        | Pars.NonAssoc,6 -> NAOP6(s)
                        | Pars.NonAssoc,7 -> NAOP7(s)
                        | Pars.NonAssoc,8 -> NAOP8(s)
                        | Pars.NonAssoc,_ -> NAOP9(s)
                      )
                    }

  | eof             { EOF }
  | eof_comment     { EOF }
  | [^'.']          { WEIRD_CHAR }
