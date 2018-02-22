%{
 open Parsing_helper

 (* Syntactic sugar *)
 let operation x op y = Expr.(App(App(Var op,x),y))

 let desugar_pattern =  Expr.(
   let add binding bindings = binding::bindings in
   let rec loop bindings pattern expr = match pattern with
     | Var "_" -> bindings
     | Var x -> add (x,expr) bindings
     | Unit -> bindings
     | Pair (x,y) ->
       let y_bindings = loop bindings y (Snd expr) in
       loop y_bindings x (Fst expr) 
     | _ -> raise (Invalid_argument ("Not a pattern: "^(show_untyped pattern)))
   in loop []
 )
   
 let desugar_where body (x,expr) = Expr.(App(Fun(x,body),expr))

 let desugar_fun pattern body = Expr.(
   match pattern with
   | Var x -> Fun(x,body)
   | Unit -> Fun("()",body)
   | pattern ->
     let x = "$0" in
     let bindings = desugar_pattern pattern (Var x) in
     let body = List.fold_left desugar_where body bindings in
     Fun(x,body)
 )

 let rec desugar_def params body = Expr.(match params with
   | Var x -> (x, body)
   | App (params, Var x) -> desugar_def params (Fun(x,body)) 
   | App (params, pattern) -> desugar_def params (desugar_fun pattern body)
   | expr -> raise (Invalid_argument ("Not a definition name: "^(show_untyped expr))) 
 )

 let rec desugar_tuple = Expr.(function
   | [] -> Unit
   | [e] -> e
   | [e;f] -> Pair(e,f)
   | e::f -> Pair(e, desugar_tuple f)
 )

  let empty_record = Expr.EmptyRecord
  let empty_case = Expr.EmptyCase

  let rec desugar_record base = Expr.(function
    | [] -> base
    | (Some name,expr)::fields -> desugar_record (Record (base, name, expr)) fields 
    | (None,expr)::_ -> raise (Invalid_argument ("Not a definition name: "^(show_untyped expr)))
  )

  let rec desugar_case cases otherwise = Expr.(match cases with
    | [] -> otherwise
    | (tag,Fun(x,body))::cases -> desugar_case cases (Case (tag, x, body, otherwise)) 
    | (_,expr)::_ -> raise (Invalid_argument ("Not a case definition: "^(show_untyped expr)))
  )
  let desugar_case cases otherwise =
    desugar_case (List.rev cases) otherwise

  let desugar_if_then_else test if_true if_false = Expr.(
    App ( App ( App (
      Var "if_then_else",
        test),
        Fun ("()", if_true)),
        Fun ("()", if_false))
  )

  let desugar_list = Expr.(
    List.fold_left (fun xs x -> App (App (Var "sonc", xs), x)) (Var "empty_sequence") 
  )

  let desugar_gen pattern body = Expr.(
    let x = "$x" in
    let r = "$r" in
    let extended_record = match pattern with
      | Var p -> Record (Var r, p, Var x)
      | pattern ->
        let bindings = desugar_pattern pattern (Var x) in
        List.fold_left (fun r (p,p_e) -> Record (r, p, p_e)) (Var r) bindings
    in
    App (Var "flat_map",
         Fun (r,
              App (App (Var "map", Fun (x, extended_record)),
                   body)))
  )

  let desugar_sequence combinator = Expr.(function
    | [] -> Unit
    | e::es ->
      List.fold_left (fun seq expr -> App(App(Var combinator, seq), expr)) e es
  )
%}

%token INFIX_CMD INFIXL_CMD INFIXR_CMD

%token <string> VAR
%token <string> SYM

%token <string> LAOP0
%token <string> LAOP1
%token <string> LAOP2
%token <string> LAOP3
%token <string> LAOP4
%token <string> LAOP5
%token <string> LAOP6
%token <string> LAOP7
%token <string> LAOP8
%token <string> LAOP9

%token <string> RAOP0
%token <string> RAOP1
%token <string> RAOP2
%token <string> RAOP3
%token <string> RAOP4
%token <string> RAOP5
%token <string> RAOP6
%token <string> RAOP7
%token <string> RAOP8
%token <string> RAOP9

%token <string> NAOP0
%token <string> NAOP1
%token <string> NAOP2
%token <string> NAOP3
%token <string> NAOP4
%token <string> NAOP5
%token <string> NAOP6
%token <string> NAOP7
%token <string> NAOP8
%token <string> NAOP9

%token <string> STR
%token <string> NUM
%token MINUS
%token IF THEN ELSE
%token ALL BAR
%token COMMA DOT
%token ARROW REV_ARROW
%token COLON EQ
%token L_PAR R_PAR
%token L_CURL R_CURL
%token L_BRACKET R_BRACKET
%token WITH
%token SEQ
%token CASE
%token OF
%token CASES
%token SEP LNSEP
%token EOF
%token WEIRD_CHAR

%start main expr
%type <(string option * Expr.untyped_expr) list> main
%type <Expr.untyped_expr> expr

%nonassoc EQ
%left LAOP0
%right RAOP0
%nonassoc NAOP0
%right ARROW COLON
%right WITH
%left REV_ARROW
%left LAOP1
%right RAOP1
%nonassoc NAOP1
%left LAOP2
%right RAOP2
%nonassoc NAOP2
%left LAOP3
%right RAOP3
%nonassoc NAOP3
%left LAOP4
%right RAOP4
%nonassoc NAOP4
%left LAOP5
%right RAOP5
%nonassoc NAOP5
%left MINUS             /* + - */
%left LAOP6
%right RAOP6
%nonassoc NAOP6
%left LAOP7             /* * / */
%right RAOP7
%nonassoc NAOP7
%left LAOP8
%right RAOP8
%nonassoc NAOP8
%left LAOP9
%right RAOP9
%nonassoc NAOP9
%nonassoc UMINUS
%left DOT

%%

main:
| cmdexprseq { $1 }
;

cmd:
| INFIX_CMD  NUM name_or_operator { add_operator ($3, NonAssoc, int_of_string $2) }
| INFIXL_CMD NUM name_or_operator { add_operator ($3, LeftAssoc, int_of_string $2) }
| INFIXR_CMD NUM name_or_operator { add_operator ($3, RightAssoc, int_of_string $2) }
;

def:
| expr           { (None, $1) }
| expr EQ expr   { let name, expr = desugar_def $1 $3 in
                   (Some name, expr)
                 }  
;

expr:
| callseq { $1 } 
| callseq ARROW expr     { desugar_fun $1 $3 }
| callseq REV_ARROW expr { desugar_gen $1 $3 }

| expr LAOP0 expr { operation $1 $2 $3 }
| expr LAOP1 expr { operation $1 $2 $3 }
| expr LAOP2 expr { operation $1 $2 $3 }
| expr LAOP3 expr { operation $1 $2 $3 }
| expr LAOP4 expr { operation $1 $2 $3 }
| expr LAOP5 expr { operation $1 $2 $3 }
| expr LAOP6 expr { operation $1 $2 $3 }
| expr LAOP7 expr { operation $1 $2 $3 }
| expr LAOP8 expr { operation $1 $2 $3 }
| expr LAOP9 expr { operation $1 $2 $3 }

| expr RAOP0 expr { operation $1 $2 $3 }
| expr RAOP1 expr { operation $1 $2 $3 }
| expr RAOP2 expr { operation $1 $2 $3 }
| expr RAOP3 expr { operation $1 $2 $3 }
| expr RAOP4 expr { operation $1 $2 $3 }
| expr RAOP5 expr { operation $1 $2 $3 }
| expr RAOP6 expr { operation $1 $2 $3 }
| expr RAOP7 expr { operation $1 $2 $3 }
| expr RAOP8 expr { operation $1 $2 $3 }
| expr RAOP9 expr { operation $1 $2 $3 }

| expr NAOP0 expr { operation $1 $2 $3 }
| expr NAOP1 expr { operation $1 $2 $3 }
| expr NAOP2 expr { operation $1 $2 $3 }
| expr NAOP3 expr { operation $1 $2 $3 }
| expr NAOP4 expr { operation $1 $2 $3 }
| expr NAOP5 expr { operation $1 $2 $3 }
| expr NAOP6 expr { operation $1 $2 $3 }
| expr NAOP7 expr { operation $1 $2 $3 }
| expr NAOP8 expr { operation $1 $2 $3 }
| expr NAOP9 expr { operation $1 $2 $3 }

| expr MINUS expr { operation $1 "-" $3 }                              /* binary infix minus */
| MINUS expr %prec UMINUS { operation (Expr.Lit ("Int", "0")) "-" $2 } /* unary prefix minus */

| expr WITH L_CURL defseq R_CURL       { desugar_record $1 $4 }

| SEQ name_or_operator L_PAR exprseq R_PAR { desugar_sequence $2 $4 }

| CASES L_CURL case_seq R_CURL         { desugar_case $3 empty_case }
| CASE expr OF L_PAR expr R_PAR        { Expr.Switch ($2, $5) }
| CASE expr OF L_CURL case_seq R_CURL  { Expr.Switch ($2, desugar_case $5 empty_case) }

| IF expr THEN expr ELSE expr %prec EQ { desugar_if_then_else $2 $4 $6 }
;

callseq:
| value            { $1 }
| callseq value    { Expr.App($1, $2) }
;

value:
| VAR                                  { Expr.Var $1 }
| value DOT VAR                        { Expr.App(Expr.Dot ("_",$3), $1) }
/* | SYM                                  { Expr.Tag ("_",$1,Expr.Unit) } */
| SYM L_PAR exprseq R_PAR              { Expr.Tag ("_", $1, desugar_tuple $3) }
| STR                                  { Expr.Lit("String", remove_quotes $1) }
| NUM                                  { Expr.Lit("Int", $1) }
| COLON type_expr  { Expr.Shape $2 }
| L_PAR exprseq R_PAR                  { desugar_tuple $2 }
| L_PAR operator R_PAR                 { Expr.Var $2 } 
| L_PAR DOT VAR R_PAR                  { Expr.Dot ("_",$3) }
| L_CURL defseq R_CURL                 { desugar_record empty_record $2 }
| L_BRACKET exprseq R_BRACKET          { desugar_list $2 }
;

type_expr:
| SYM                                  { Datatype.Atomic $1 }
| L_PAR typeexprseq R_PAR              { Datatype.Tuple $2 }
| L_CURL typedefseq R_CURL             { Datatype.Record $2 }
| L_BRACKET type_expr R_BRACKET        { Datatype.List $2 }
;

type_def:
| VAR COLON type_expr                  { ($1, $3) }
;

cmdexprseq:
| EOF                        { [] }
| LNSEP cmdexprseq EOF       { $2 }
| revcmdexprseq EOF          { List.rev $1 }
;

revcmdexprseq:
| def                        { [$1] }
| cmd                        { [] }
| revcmdexprseq COMMA def    { $3 :: $1 }
| revcmdexprseq COMMA cmd    { $1 }
| revcmdexprseq LNSEP def    { $3 :: $1 }
| revcmdexprseq LNSEP cmd    { $1 }
| revcmdexprseq LNSEP        { $1 }
;

exprseq:
| lnsep                      { [] }
| lnsep revexprseq lnsep     { List.rev $2 }
;

revexprseq:
| expr                               { [$1] }
| revexprseq lnsep COMMA lnsep expr  { $5 :: $1 }
| revexprseq LNSEP lnsep expr        { $4 :: $1 }
;

defseq:
|                           { [] }
| lnsep revdefseq lnsep     { List.rev $2 }
;

revdefseq:
| def                              { [$1] }
| revdefseq lnsep COMMA lnsep def  { $5 :: $1 }
| revdefseq LNSEP lnsep def        { $4 :: $1 }
;

case_seq:
|                             { [] }
| lnsep revcase_seq lnsep     { List.rev $2 }
;

revcase_seq:
| case                                { [$1] }
| revcase_seq lnsep COMMA lnsep case  { $5 :: $1 }
| revcase_seq LNSEP lnsep case        { $4 :: $1 }
;

case:
| SYM ARROW expr                      { ($1,desugar_fun Expr.Unit $3) }
| SYM L_PAR exprseq R_PAR ARROW expr  { ($1,desugar_fun (desugar_tuple $3) $6) }
;

typeexprseq:
| lnsep                        { [] }
| lnsep revtypeexprseq lnsep   { List.rev $2 }
;

revtypeexprseq:
| type_expr                                   { [$1] }
| revtypeexprseq lnsep COMMA lnsep type_expr  { $5 :: $1 }
| revtypeexprseq LNSEP lnsep type_expr        { $4 :: $1 }
;

typedefseq:
|                               { [] }
| lnsep revtypedefseq lnsep     { List.rev $2 }
;

revtypedefseq:
| type_def                                  { [$1] }
| revtypedefseq lnsep COMMA lnsep type_def  { $5 :: $1 }
| revtypedefseq LNSEP lnsep type_def        { $4 :: $1 }
;

lnsep:
|               { () }
| LNSEP lnsep   { () }
| EOF           { raise UnexpectedEof }
;

name_or_operator:
| VAR      { $1 }
| operator { $1 }
;

operator:
/* | MINUS { "-" }     binary infix minus      FIXME: (-) and (-3) introduce a conflict */

| LAOP0 { $1 }
| LAOP1 { $1 }
| LAOP2 { $1 }
| LAOP3 { $1 }
| LAOP4 { $1 }
| LAOP5 { $1 }
| LAOP6 { $1 }
| LAOP7 { $1 }
| LAOP8 { $1 }
| LAOP9 { $1 }

| RAOP0 { $1 }
| RAOP1 { $1 }
| RAOP2 { $1 }
| RAOP3 { $1 }
| RAOP4 { $1 }
| RAOP5 { $1 }
| RAOP6 { $1 }
| RAOP7 { $1 }
| RAOP8 { $1 }
| RAOP9 { $1 }

| NAOP0 { $1 }
| NAOP1 { $1 }
| NAOP2 { $1 }
| NAOP3 { $1 }
| NAOP4 { $1 }
| NAOP5 { $1 }
| NAOP6 { $1 }
| NAOP7 { $1 }
| NAOP8 { $1 }
| NAOP9 { $1 }

;

%%
