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
end

module Make(Schema: Schema.S) : Query.S = struct
  include Schema
  include Var

  type 'a clause =
    | Clause: (('a,'c) var * ('a,'b) relation * ('b,'c) var) -> 'c clause

  type 'a query = 'a clause list

  let query q = q

  let (!!) x rel y = Clause (x,rel,y)
end

