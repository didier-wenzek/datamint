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

type 'a lower_bound =
  | NoLB
  | SomeGT of 'a
  | SomeGE of 'a

type 'a upper_bound =
  | NoUB
  | SomeLT of 'a
  | SomeLE of 'a

type 'a range = 'a lower_bound * 'a upper_bound

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

  let _max x y =
    if Elt.compare x y >= 0
    then x else y

  let _min x y =
    if Elt.compare x y <= 0
    then x else y

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

  let max_lower_bound a b = match a,b with
    | NoLB, a
    | a, NoLB -> a

    | SomeGT x, SomeGT y
    | SomeGE x, SomeGT y
    | SomeGT x, SomeGE y
    | SomeGE x, SomeGE y ->
      let cmp = Elt.compare x y in
      if cmp < 0 then b
      else if cmp > 0 then a
      else (match a with SomeGE _ -> b | _ -> a)

  let min_upper_bound a b = match a,b with
    | NoUB, a
    | a, NoUB -> a

    | SomeLT x, SomeLT y
    | SomeLE x, SomeLT y
    | SomeLT x, SomeLE y
    | SomeLE x, SomeLE y ->
      let cmp = Elt.compare x y in
      if cmp < 0 then a
      else if cmp > 0 then b
      else (match a with SomeLE _ -> b | _ -> a)

  let inter_bounds (min1,max1) (min2,max2) =
    (max_lower_bound min1 min2, min_upper_bound max1 max2)

  let is_empty (min,max) = match min, max with
    | SomeGT x, SomeLT y
    | SomeGT x, SomeLE y
    | SomeGE x, SomeLT y -> ge x y
    | SomeGE x, SomeLE y -> gt x y
    | NoLB, _
    | _, NoUB -> false

  let increasing_lower_bounds (min1,_) (min2,_) = match min1, min2 with
    | NoLB, _ -> -1
    | _, NoLB -> +1

    | SomeGT x, SomeGT y
    | SomeGE x, SomeGT y
    | SomeGT x, SomeGE y
    | SomeGE x, SomeGE y ->
      let cmp = Elt.compare x y in
      if cmp <> 0 then cmp
      else (match min1,min2 with
        | SomeGT _, SomeGE _ -> -1
        | SomeGE _, SomeGT _ -> +1
        | _ -> 0)

  let smdnf =
    let full = (NoLB,NoUB) in
    let inter r = function
      | Full -> r
      | EQ x -> inter_bounds (SomeGE x, SomeLE x) r
      | LT x -> inter_bounds (NoLB    , SomeLT x) r
      | LE x -> inter_bounds (NoLB    , SomeLE x) r
      | GT x -> inter_bounds (SomeGT x, NoUB    ) r
      | GE x -> inter_bounds (SomeGT x, NoUB    ) r
      | _ -> assert false
    in
    let union s = function
      | r when is_empty r -> s
      | (NoLB,NoUB) -> [full]
      | r -> r::s
    in
    fun e -> dnf e
    |> List.map (List.fold_left inter full)
    |> List.sort increasing_lower_bounds
    |> List.fold_left union []

end
