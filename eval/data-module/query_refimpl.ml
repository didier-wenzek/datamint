open Util

module Var = struct
  type ('a,'b) var =
    | Ignore
    | Val of 'a
    | Var of { key: int; getter: 'b -> 'a }

  let new_key =
    let next_key = ref 0 in
    fun () ->
      let key = !next_key in
      next_key := key + 1;
      key

  let var getter =
    let key = new_key () in
    Var { key; getter; }

  let equals x y = match x,y with
    | Var x, Var y -> x.key = y.key
    | _ -> false

  let __ = Ignore
  let value x = Val x
  let var1 () = var id
  let var2 () = (var fst, var snd)

  let fst3 (x,y,z) = x
  let snd3 (x,y,z) = y
  let thr3 (x,y,z) = z
  let var3 () = (var fst3, var snd3, var thr3)

  let nth1_4 (x1,x2,x3,x4) = x1
  let nth2_4 (x1,x2,x3,x4) = x2
  let nth3_4 (x1,x2,x3,x4) = x3
  let nth4_4 (x1,x2,x3,x4) = x4
  let var4 () = (var nth1_4, var nth2_4, var nth3_4, var nth4_4)

  let here (x,y) = x
  let next f (x,y) = f y

  let var5 () =
    let x1 = here in
    let x2 = next here in
    let x3 = next (next here) in
    let x4 = next (next (next here)) in
    let x5 = next (next (next (next here))) in
    (var x1, var x2, var x3, var x4, var x5)
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

  type ('a,'b) selection = 'b -> 'a

  type 'a query =
    | Query: ('a,'b) selection * 'b clause list -> 'a query

  let all = id

  let select_var = function
    | Var x -> x.getter
    | Val x -> fun _ -> x
    | Ignore -> raise (Invalid_argument "An ignored variable cannot be selected")

  let (!$) = select_var

  let ($) selection var =
    let var = select_var var in
    fun c -> (selection c, var c)

  let select selection query = Query (selection, query)

  let (!!) x rel y = Clause (x,rel,y)

  let run q = raise (Invalid_argument "Not implemented")
end

