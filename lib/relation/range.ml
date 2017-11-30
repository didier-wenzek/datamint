type 'a t =
  | Empty
  | Full
  | EQ of 'a
  | NE of 'a
  | LT of 'a
  | LE of 'a
  | GT of 'a
  | GE of 'a

type 'a expr =
  | In of 'a t
  | And of 'a expr * 'a expr
  | Or of 'a expr * 'a expr
  | Not of 'a expr

let rec remove_negation = function
  | Not (Not e) -> remove_negation e
  | Not (And (e,f)) -> Or (remove_negation (Not e), remove_negation (Not f))
  | Not (Or (e,f)) -> And (remove_negation (Not e), remove_negation (Not f))
  | Not (In (EQ x)) -> Or (In (LT x), In (GT x))
  | Not (In r) -> In (negate r)
  | And (e,f) -> And (remove_negation e, remove_negation f)
  | Or (e,f) -> Or (remove_negation e, remove_negation f)
  | in_r -> in_r

and negate = function
  | Empty -> Full
  | Full -> Empty
  | EQ x -> NE x
  | NE x -> EQ x
  | LT x -> GE x
  | LE x -> GT x
  | GT x -> LE x
  | GE x -> LT x
  
let dnf_expr =
  let rec rewrite = function
    | Or (e,f) -> disjonction (rewrite e) (rewrite f)
    | And (e,f) -> distribute (rewrite e) (rewrite f)
    | literal -> literal

  and disjonction x y = match x, y with
    | (In Full, _) | (_, In Full) -> In Full
    | (In Empty, x) | (x, In Empty) -> x
    | _ -> Or (x,y)

  and conjonction x y = match x, y with
    | (In Empty, _) | (_, In Empty) -> In Empty
    | (In Full, x) | (x, In Full) -> x
    | _ -> And (x,y)

  and distribute e f = match e, f with
    | Or (e1,e2), f -> disjonction (distribute e1 f) (distribute e2 f)
    | e, Or (f1,f2) -> disjonction (distribute e f1) (distribute e f2)
    | e, f -> conjonction e f
  in

  fun e -> rewrite (remove_negation e)

let dnf =
  let rec hoist_and ranges = function
    | In Full -> assert (ranges = []); []
    | In range -> range::ranges
    | And (x,y) -> hoist_and (hoist_and ranges y) x
    | _ -> assert false
  in
  let rec hoist_or clauses = function
    | In Empty -> assert (clauses = []); []
    | Or (x,y) -> hoist_or (hoist_or clauses y) x
    | clause -> (hoist_and [] clause)::clauses
  in
  fun e -> hoist_or [] (dnf_expr e)

module Make(Elt: Map.OrderedType) = struct

  type elt = Elt.t

  let eq x y = Elt.compare x y = 0
  let ne x y = Elt.compare x y <> 0
  let lt x y = Elt.compare x y < 0
  let le x y = Elt.compare x y <= 0
  let gt x y = Elt.compare x y > 0
  let ge x y = Elt.compare x y >= 0

  let contain x = function 
    | Empty -> false
    | Full -> true
    | EQ y -> eq x y
    | NE y -> ne x y
    | LT y -> lt x y
    | LE y -> le x y
    | GT y -> gt x y
    | GE y -> ge x y

  let overlap r s = match r,s with
    | Empty, _
    | _, Empty -> false

    | Full, _
    | _, Full -> true

    | EQ x, r
    | r, EQ x -> contain x r

    | GT x, LT y 
    | GT x, LE y 
    | GE x, LT y 
    | LT y, GT x
    | LE y, GT x
    | LT y, GE x -> lt x y

    | GE x, LE y
    | LE y, GE x -> le x y

    | _, _ -> true

  module Expr = struct

    type bnf =
      | Union of union
      | Inter of inter

    and union =
      | FullSet
      | MaxUnion of elt * bool * inter

    and inter =
      | EmptySet
      | MinInter of elt * bool * union

    type nf =
      | EmptyRange
      | FullRange
      | UnionMax of elt * bool * nf  (* UnionMax(x_max, false, xs) = {x | x < x_max} union xs
                                        UnionMax(x_max, true, xs) = {x | x <= x_max} union xs *)
      | InterMin of elt * bool * nf  (* InterMin(x_min, false, xs) = {x | x > x_min} inter xs 
                                        InterMin(x_min, true, xs) = {x | x >= x_min} inter xs *)

    let rec inter_min x include_x = function
      | EmptyRange -> EmptyRange
      | FullRange -> InterMin(x,include_x,FullRange)

      | UnionMax (y, include_y, oys) as ys ->
        let cmp = Elt.compare x y in
        if cmp = 0
        then
          if include_x && include_y
          then InterMin(x, true, ys)
          else oys
        else
          if cmp < 0
          then InterMin(x,include_x,ys) 
          else inter_min x include_x oys
      
      | InterMin (y, include_y, oys) as ys ->
        let cmp = Elt.compare x y in
        if cmp = 0
        then
          InterMin(x, include_x && include_y, oys)
        else
          if cmp > 0
          then inter_min x include_x oys 
          else ys

    let clause = function
      | Empty -> EmptyRange
      | Full -> FullRange
      | EQ x -> InterMin(x, true,  UnionMax(x, true, EmptyRange))
      | NE x -> UnionMax(x, false, InterMin(x, false, FullRange))
      | LT x -> UnionMax(x, false, EmptyRange)
      | LE x -> UnionMax(x, true, EmptyRange)
      | GT x -> InterMin(x, false, FullRange)
      | GE x -> InterMin(x, true, FullRange)

    let rec inter xs ys = match xs, ys with
      | EmptyRange, _ | _, EmptyRange -> EmptyRange
      | FullRange, e | e, FullRange -> e

      | UnionMax (x,include_x,oxs), UnionMax (y,include_y,oys) ->
        let cmp = Elt.compare x y in
        if cmp = 0
        then UnionMax(x, include_x && include_y, inter oxs oys)
        else
          if cmp < 0
          then UnionMax(x, include_x, inter oxs (inter_min x (not include_x) ys))
          else UnionMax(y, include_y, inter oys (inter_min y (not include_y) xs))

      | InterMin (x,include_x,oxs), InterMin (y,include_y,oys) ->
        let cmp = Elt.compare x y in
        if cmp = 0
        then InterMin(x, include_x && include_y, inter oxs oys)
        else
          if cmp > 0
          then inter xs (inter_min x include_x ys)
          else inter ys (inter_min y include_y xs)

      | _ -> FullRange
      
    let rec union xs ys = match xs, ys with
      | EmptyRange, e | e, EmptyRange -> e
      | FullRange, _ | _, FullRange -> FullRange

      | UnionMax (x,include_x,oxs), UnionMax (y,include_y,oys) ->
        let cmp = Elt.compare x y in
        if cmp = 0
        then UnionMax(x, include_x || include_y, union oxs oys)
        else
          if cmp > 0
          then UnionMax(x, include_x, union oxs (inter_min x (not include_x) ys))
          else UnionMax(y, include_y, union oys (inter_min y (not include_y) xs))
 
      | InterMin (x,include_x,oxs), InterMin (y,include_y,oys) ->
        let cmp = Elt.compare x y in
        if cmp = 0
        then InterMin(x, include_x || include_y, union oxs oys)
        else
          if cmp < 0
          then union xs (inter_min x include_x ys)
          else union ys (inter_min y include_y xs)

      | _ -> FullRange
      
    let fold empty full union inter clause =
      let rec hoist_and ranges = function
        | In Full -> full
        | In range -> inter ranges (clause range)
        | And (x,y) -> hoist_and (hoist_and ranges y) x
        | _ -> assert false
      in
      let rec hoist_or clauses = function
        | In Empty -> empty
        | Or (x,y) -> hoist_or (hoist_or clauses y) x
        | clause -> union clauses (hoist_and FullRange clause)
      in
      fun e -> hoist_or EmptyRange (dnf_expr e)

    let nf =
      fold EmptyRange FullRange union inter clause 

  end
   
end
