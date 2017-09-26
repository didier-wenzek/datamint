exception UnexpectedEof
exception SyntaxError of int * int * string

let remove_quotes s =
  let len = String.length s in
  String.sub s 1 (len - 2)
  |> Scanf.unescaped

type assoc_kind = LeftAssoc | NonAssoc | RightAssoc
type operator = {
  op_name: string;
  op_prio: int;
  op_assoc: assoc_kind;
}

let add_operator, get_operator =
  let table = Hashtbl.create 53 in
  let add_operator (name,assoc,prio) = Hashtbl.replace table name {op_name=name;op_prio=prio;op_assoc=assoc} in
  let get_operator name =
    try let op = Hashtbl.find table name in Some ((op.op_assoc,op.op_prio))
    with Not_found -> None in
  (add_operator, get_operator)                                                                                                                                                                             
