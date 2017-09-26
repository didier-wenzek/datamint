exception Already_bound

module type Expr = sig
  type 'var u_expr

  val to_var: 'var u_expr -> 'var option
end

module VarExpr(E: Expr) = struct

  type expr = var E.u_expr
  and var = value ref
  and value =
    | Unset
    | Var of var
    | Val of expr

  let new_var () = ref Unset

  let rec collapse x = match !x with
    | Var y ->
      assert (x != y);
      let  z = collapse y in
      x := Var z;
      z
    | _ -> x

  let equal x y =
    let x = collapse x in
    let y = collapse y in
    x == y

  let is_bound x = match !(collapse x) with
    | Val _ -> true
    | _ -> false

  let set_var x y =
    if x != y
    then x := Var y
    
  let set_value x v =
    let x = collapse x in
    match E.to_var v with
    | Some y ->
      let y = collapse y in
      set_var x y
    | None -> x := Val v

  let merge combine assignable x y =
    let x = collapse x in
    let y = collapse y in
    match !x, !y with
    | Val a, Val b ->
      let c = combine a b in
      if assignable x c
      then
        set_value x c;
        set_var y x
    | Val a, Unset ->
      if assignable y a
      then
        set_var y x
    | Unset, Val b ->
      if assignable x b
      then
        set_var x y
    | Unset, Unset ->
      set_var y x
    | _ -> assert false

  let update combine assignable x b =
    let x = collapse x in
    match !x with
    | Val a ->
      let c = combine a b in
      if assignable x c
      then
        set_value x c
    | Unset ->
      if assignable x b
      then
        set_value x b
    | _ -> assert false

  let assign_value x v =
    let x = collapse x in
    match !x with
    | Unset -> x := Val v
    | _ -> raise Already_bound

  let unify x y =
    let x = collapse x in
    let y = collapse y in
    match !x with
    | Unset -> x := Var y
    | _ -> (
      match !y with
      | Unset -> y := Var x
      | _ -> raise Already_bound
    )

  let assign x v = match E.to_var v with
    | Some y -> unify x y
    | None -> assign_value x v

  let value x =
    let x = collapse x in match !x with
    | Val v -> Some v
    | Unset -> None
    | _ -> assert false 

end
