open OUnit2
open Format
open Interpreter
module Comp = Core_functional_language.Make(Semantics_witness.TaglessFinal)

let normalize =
  let simplify regex sep =
    let split = Str.split (Str.regexp regex) in
    fun s -> String.concat sep (split s)
  in
  let simplify_char sep =
    simplify ("[ ]*" ^ sep ^ "[ ]*") sep
  in
  let space = simplify "[ ]+" " " in
  let comma = simplify_char "," in
  let eq = simplify_char "=" in
  let o_par = simplify_char "(" in
  let c_par = simplify_char ")" in
  let o_curl = simplify_char "{" in
  let c_curl = simplify_char "}" in
  let o_bra = simplify "[ ]*[[][ ]*" "[" in
  let c_bra = simplify "[ ]*[]][ ]*" "]" in
  fun s -> s
    |> comma
    |> eq
    |> o_par
    |> c_par
    |> o_curl
    |> c_curl
    |> o_bra
    |> c_bra
    |> space

let check_result expected_result (oname, ctype, cexpr) =
  let kind, value = Comp.show_typed_value ctype cexpr in
  let actual_result = match oname with
    | None  -> value
    | Some name -> sprintf "%s %s %s" name kind value
  in
  let expected_result = normalize expected_result in
  let actual_result = normalize actual_result in
  assert_equal expected_result actual_result

let rec check_results expected_results actual_values =
  match expected_results, actual_values with
  | [], [] -> ()
  | [], _ -> assert_failure "unexpected result"
  | _, [] -> assert_failure "missing result"
  | expected::expected_results , actual::actual_values ->
    let () = check_result expected actual in
    check_results expected_results actual_values

let test_eval str expected_results =
  let name = "eval "^str in
  let env = Comp.initial_env in
  let test _test_ctxt =
    let exprs = Comp.parse_string str in
    let (_, actual_values) = Comp.eval_exprs env exprs in
    check_results expected_results actual_values
  in name >:: test

let test_format format shape value =
  let name = format ^ shape in
  let code =
    sprintf "map (%s_decode %s) (%s_encode %s %s)" format shape format shape value
  in
  let expected_result =
    sprintf "[[%s]]" value
  in
  let env = Comp.initial_env in
  let test _test_ctxt =
    let exprs = Comp.parse_string code in
    let (_, actual_values) = Comp.eval_exprs env exprs in
    check_results [expected_result] actual_values
  in name >:: test

let test_error str expected_error =
  let name = "error "^str in
  let env = Comp.initial_env in
  let test _test_ctxt =
    try
      let exprs = Comp.parse_string str in
      let _ = Comp.eval_exprs_bg env exprs in
      assert_failure "expected error not raised"
    with e -> assert_equal expected_error (Comp.error_msg e)
  in name >:: test

let suite =
"suite">:::[
  test_eval "x = add 1 one"                                                         [ "x = 2" ];
  test_eval "x = add one one"                                                       [ "x = 2" ];
  test_eval "x = 10, succ = add one, succ x"                                        [ "x = 10"; "succ : Int -> Int"; "11"];
  test_eval "!infix_left 6 +, x+y = add x y, succ(x) = x + 1, succ 12"              [ "+ : Int -> Int -> Int"; "succ : Int -> Int"; "13" ];
  test_eval "(x -> y -> add x y)"                                                   [ "Int -> Int -> Int" ];
  test_eval "(x -> y -> add x y) 14"                                                [ "Int -> Int" ];
  test_eval "(x -> y -> add x y) 14 6"                                              [ "20" ];
  test_eval "(x -> x)"                                                              [ "forall a. a -> a" ];
  test_eval "(x -> x) 12"                                                           [ "12" ];
  test_eval "(x -> x) true"                                                         [ "true" ];
  test_eval "(x -> x) add"                                                          [ "Int -> Int -> Int" ];
  test_eval "(x -> x) add 1 2"                                                      [ "3" ];
  test_eval "(x -> x) (x -> x)"                                                     [ "forall a. a -> a" ];
  test_eval "(x -> x) (x -> x) 1"                                                   [ "1" ];
  test_eval "id = x -> x, id 3"                                                     [ "id : forall a. a -> a"; "3" ];
  test_eval "id = x -> x, id 3, id true"                                            [ "id : forall a. a -> a"; "3"; "true" ];
  test_eval "id = x -> x, id 3, id"                                                 [ "id : forall a. a -> a"; "3"; "forall a. a -> a" ];
  test_eval "comp f g = x -> f(g(x))"                                               [ "comp : forall a, b, c. (a -> b) -> (c -> a) -> c -> b" ];
  test_eval "than f g x = g(f(x))"                                                  [ "than : forall a, b, c. (a -> b) -> (b -> c) -> a -> c" ];
  test_eval "succ x = add 1 x, than f g x = g(f(x)), than succ succ 6"              [ "succ : Int -> Int";
                                                                                            "than : forall a, b, c. (a -> b) -> (b -> c) -> a -> c";
                                                                                            "8" ];
  test_eval "fst"                                                                   [ "forall a, b. (a, b) -> a" ];
  test_eval "snd"                                                                   [ "forall a, b. (a, b) -> b" ];
  test_eval "fst (add 1 1, 8)"                                                      [ "2" ];
  test_eval "snd (add 1 1, 8)"                                                      [ "8" ];
  test_eval "f(x,y) = x, f(1,2), f(3,true)"                                         [ "f : forall a, b. (a, b) -> a"; "1"; "3" ];
  test_eval "f(x,y) = y, f(1,2), f(3,true)"                                         [ "f : forall a, b. (a, b) -> b"; "2"; "true" ];
  test_eval "((x,y) -> add x y)(3,7)"                                               [ "10" ];
  test_eval "{ x = 1, y = 2 }"                                                      [ "{x = 1, y = 2}" ];
  test_eval "{ x = 1, y = 2 }.x"                                                    [ "1" ];
  test_eval "{ x = 1, y = 2 }.y"                                                    [ "2" ];
  test_eval "{ y = 1, x = 2 }.x"                                                    [ "2" ];
  test_eval "{ y = 1, x = 2 }.y"                                                    [ "1" ];
  test_eval "give_name n r = r with { name = n }"                                   [ "give_name : forall a, b with { ... }. a -> b -> b with { name:a }" ];
  test_eval "give_name n r = r with { name = n }, give_name 12 { x = 1, y = 2 }"    [ "give_name : forall a, b with { ... }. a -> b -> b with { name:a }" ;
                                                                                            "{x = 1, y = 2, name = 12}" ];
  test_eval "give_name n r = r with { name = n }, give_name 12 {}"                  [ "give_name : forall a, b with { ... }. a -> b -> b with { name:a }" ;
                                                                                            "{name = 12}" ];
  test_eval "n -> r -> r with { name = n }"                                         [ "forall a, b with { ... }. a -> b -> b with { name:a }" ];
  test_eval "n -> (r -> r with { name = n })"                                       [ "forall a, b with { ... }. a -> b -> b with { name:a }" ];
  test_eval "(n -> (r -> r with { name = n })) 12"                                  [ "forall a with { ... }. a -> a with { name:Int }" ];
  test_eval "(n -> (r -> r with { name = n })) 12 { x = 1, y = 2 }"                 [ "{x = 1, y = 2, name = 12}" ];
  test_eval "(.name)"                                                               [ "forall a with { name:b, ... }, b. a -> b" ];
  test_eval "(.b) { a = 1, b = 12 , c = 1 }"                                        [ "12" ];
  test_eval "(.b) { c = 1, b = 12 , a = 1 }"                                        [ "12" ];
  test_eval "(.c) { a = 1, c = 12 , b = 1 }"                                        [ "12" ];
  test_eval "r = { f(x,y) = add x y }, r.f, r.f(12,13)"                             [ "r : { f:(Int, Int) -> Int }"; "(Int, Int) -> Int"; "25" ];
  test_eval "(r -> add (r.x) (r.y)) { y=3, z=2, x=12 }"                             [ "15" ];
  test_eval "(r -> add r.x r.y) { y=3, z=2, x=12 }"                                 [ "15" ];
  test_eval "sum r = add (r.x) (r.y)"                                               [ "sum : forall a with { x:Int, y:Int, ... }. a -> Int" ];
  test_eval "sum r = add (r.x) (r.y), sum { y=3, z=2, x=12 }"                       [ "sum : forall a with { x:Int, y:Int, ... }. a -> Int"; "15" ];
  test_eval "(r -> add (r.x) (r.y.z)) { foo = 3, x = 1 , y = { bar = 7, z = 2 }}"   [ "3" ];
  test_eval "f r = add (r.x) (r.y.z), f { x = 1 , y = { z = 2 }}"                   [ "f : forall a with { x:Int, y:b, ... }, b with { z:Int, ... }. a -> Int"; "3" ];
  test_eval "ext r = r with { x = 1 }, ext { x = 2 }, (ext { x = 2 }).x"            [ "ext : forall a with { ... }. a -> a with { x:Int }"; "{x = 2, x = 1}"; "1" ];
  test_eval "f r = r with { x = 2 * r.x, y = 2 * r.y }"                             [ "f : forall a with { x:Int, y:Int, ... }. a -> a with { x:Int, y:Int }" ];
  test_eval "range 1 5"                                                             [ "[1, 2, 3, 4, 5]" ];
  test_eval "repeat 5 3"                                                            [ "[3, 3, 3, 3, 3]" ];
  test_eval "iterate 10 (add 9) 0"                                                  [ "[0, 9, 18, 27, 36, 45, 54, 63, 72, 81]" ];
  test_eval "sum = monoid 0 (+), reduce sum (range 1 10)"                           [ "sum : Reducer(Int, Int, Int)"; "55" ];
  test_eval "sum = monoid 0 (+), reduce sum (map ((*) 2) (range 1 10))"             [ "sum : Reducer(Int, Int, Int)"; "110" ];
  test_eval "reduce (monoid 0 (+)) (flat_map (range 1) (range 1 10))"               [ "220" ];
  test_eval "reduce (monoid 0 (+)) (map (x -> x * (11 - x)) (range 1 10))"          [ "220" ];
  test_eval "reduce (monoid 0 (+)) (filter (x -> (x mod 2) < 1) (range 1 10))"     [ "30" ];
  test_eval "x_in = map (x -> { x = x }), x_in (range 1 3)"                         [ "x_in : forall a. [a] -> [{ x:a }]"; "[{x = 1}, {x = 2}, {x = 3}]" ];
  test_eval "add_y_in ys = flat_map (r -> map (y -> r with { y = y }) ys)"          [ "add_y_in : forall a, b with { ... }. [a] -> [b] -> [b with { y:a }]" ];
  test_eval "x_in = map (x -> { x = x })
             add_y_in ys = flat_map (r -> map (y -> r with { y = y }) ys)
             add_y_in (range 1 2) (x_in (range 1 3))"                               [ "x_in : forall a. [a] -> [{ x:a }]";
                                                                                      "add_y_in : forall a, b with { ... }. [a] -> [b] -> [b with { y:a }]";
                                                                                      "[{x = 1, y = 1}, {x = 1, y = 2}, {x = 2, y = 1}, {x = 2, y = 2}, {x = 3, y = 1}, {x = 3, y = 2}]" ];
  test_eval "!infix_left 0 |>, x|>f = f(x)
             range 1 10 |> map (range 1) |> flat_map take_last"                     [ "|> : forall a, b. a -> (a -> b) -> b";
                                                                                      "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]" ];
  test_eval "(x in (range 1 2)) [{}]"                                               [ "[{x = 1}, {x = 2}]" ];
  test_eval "((x,y) in [(1,23), (5,8)]) [{}]"                                       [ "[{x = 1, y = 23}, {x = 5, y = 8}]" ];
  test_eval "((x,_) in [(1,23), (5,8)]) [{}]"                                       [ "[{x = 1}, {x = 5}]" ];
  test_eval "((_,y) in [(1,23), (5,8)]) [{}]"                                       [ "[{y = 23}, {y = 8}]" ];
  test_eval "((_,_) in [(1,23), (5,8)]) [{}]"                                       [ "[(), ()]" ];
  test_eval "seq + (1,2,3,5,6,7)"                                                   [ "24" ];
  test_eval "seq + (1)"                                                             [ "1" ];
  test_eval "seq + ()"                                                              [ "()" ];
  test_eval "and_red = monoid_with_maximum true (and) false
             forall p xs = reduce and_red (map p xs)
             forall ((>) 10) [2, 4, 6, 8]
             forall ((>) 10) [2, 11, 6, 8]
             forall ((>) 10) (flat_map (range 1) (range 0 1000000))"                [ "and_red : Reducer(Bool, Bool, Bool)";
                                                                                      "forall : forall a. (a -> Bool) -> [a] -> Bool";
                                                                                      "true";
                                                                                      "false";
                                                                                      "false" ];
  test_eval "take 2 (range 1 10000000)"                                             [ "[1, 2]" ];
  test_eval "drop 8 (range 1 10)"                                                   [ "[9, 10]" ];
  test_eval "take_while (x -> x < 4) (range 1 10)"                                  [ "[1, 2, 3]" ];
  test_eval "drop_while (x -> x < 8) (range 1 10)"                                  [ "[8, 9, 10]" ];
  test_eval "unique [1,2,2,3,2,1,3]"                                                [ "[1, 2, 3]" ];
  test_eval "dedupe [1,2,2,3,2,1,3]"                                                [ "[1, 2, 3, 2, 1, 3]" ];
  test_eval "dedupe []"                                                             [ "[]" ];
  test_eval "dedupe [1,2,3,2,1]"                                                    [ "[1, 2, 3, 2, 1]" ];
  test_eval "interpose 0 [1,2,3]"                                                   [ "[1, 0, 2, 0, 3]" ];
  test_eval "\"a\" ++ \"b\" ++ \"c\""                                               [ "\"abc\"" ];
  test_eval "\"éàç\""                                                               [ "\"éàç\"" ];
  test_eval "\"\\\\\""                                                              [ "\"\\\\\"" ];
  test_eval "\"\\\"\""                                                              [ "\"\\\"\"" ];
  test_eval ":Int"                                                                  [ "Shape(Int)" ];
  test_eval ":(Int)"                                                                [ "Shape(Int)" ];
  test_eval ":(Int,String)"                                                         [ "Shape((Int, String))" ];
  test_eval ":(Int,String,Bool)"                                                    [ "Shape((Int, String, Bool))" ];
  test_eval ":[Int]"                                                                [ "Shape([Int])" ];
  test_eval ":{ foo: Int, bar: String }"                                            [ "Shape({ foo:Int, bar:String })" ];
  test_eval ":{ foo: Int, bar: String, tags: [String] }"                            [ "Shape({ foo:Int, bar:String, tags:[String] })" ];
  test_eval "json_decode :{ foo: Int, bar : String } \"{foo:1, bar:\\\"xox\\\"}\""  [ "[{foo = 1, bar = \"xox\"}]" ];
  test_eval "json_decode :{ foo: Int, bar : String } \"garbage\""                   [ "[]" ];
  test_eval "json_decode :{ foo: Int, bar : String } \"{foo:1}\""                   [ "[]" ];
  test_eval "json_decode :[Int] \"[1,2,3]\""                                        [ "[[1, 2, 3]]" ];
  test_eval "json_decode :(Int,Int,Int) \"[1,2,3]\""                                [ "[(1, 2, 3)]" ];
  test_eval "json_encode :Int 3"                                                    [ "[\"3\"]" ];
  test_eval "json_encode :(Int,String) (3,\"dede\")"                                [ "[\"(3,\\\"dede\\\")\"]" ];
  test_eval "json_encode :[Int] [1,2,3]"                                            [ "[\"[1,2,3]\"]" ];
  test_eval "order_by p x y = p(x) > p(y)"                                          [ "order_by : forall a, b. (a -> b) -> a -> a -> Bool" ];
  test_eval "rolling (monoid 0 (+)) (range 1 5)"                                    [ "[1, 3, 6, 10, 15]" ];
  test_eval "reverse = transduce [] (x -> xs -> (cons x xs, [])) (xs -> xs)
             reverse (range 0 9)"                                                   [ "reverse : forall a. [a] -> [a]";
                                                                                      "[9, 8, 7, 6, 5, 4, 3, 2, 1, 0]" ];
  test_eval "(fst (x -> x, y -> y))(1)"                                             [ "1" ];
  test_eval "{ compose = f -> g -> x -> g(f(x)), id(x) = x }.id 4"                  [ "4" ];
  test_eval "r = { compose = f -> g -> x -> g(f(x)), id(x) = x }, r.id 4"           [ "r : forall a, b, c, d. { compose:(a -> b) -> (b -> c) -> a -> c, id:d -> d }"; "4" ];
  test_eval "unique []"                                                             [ "[]" ];
  test_eval "e = unique [], e"                                                      [ "e = []"; "[]" ];
  test_eval "string_split \",\" \"a,b,c\""                                          [ "[\"a\", \"b\", \"c\"]" ];
  test_eval "string_split \",\" \"a,,,c\""                                          [ "[\"a\", \"\", \"\", \"c\"]" ];
  test_eval "string_split \",+\" \"a,,,c\""                                         [ "[\"a\", \"c\"]" ];
  test_eval "string_extract \"[a-z]\" \"a,b,c\""                                    [ "[\"a\", \"b\", \"c\"]" ];
  test_eval "string_extract \"[a-z]\" \"   a,  b, c\""                              [ "[\"a\", \"b\", \"c\"]" ];
  test_eval "[]"                                                                    [ "[]" ];
  test_eval "() -> 2"                                                               [ "() -> Int" ];
  test_eval "if true then 1 else 2"                                                 [ "1" ];
  test_eval "if false then 1 else 2"                                                [ "2" ];
  test_eval "if true then 1 else 2 + 1"                                             [ "1" ];
  test_eval "(if true then 1 else 2) + 1"                                           [ "2" ];
  test_eval "if false then 1 else 2 + 1"                                            [ "3" ];
  test_eval "(if false then 1 else 2) + 1"                                          [ "3" ];
  test_eval "(() -> 1) ()"                                                          [ "1" ];
  test_eval "case A(2) of { A(x) -> x + 1, B(x) -> x * x }"                         [ "3" ];
  test_eval "case B(2) of { A(x) -> x + 1, B(x) -> x * x }"                         [ "4" ];
  test_eval "cases { A(x) -> x + 1, B(x) -> x * x, C(_) -> 0 }"                     [ "forall a. either { A(Int), B(Int), C(a) } -> Int" ];
  test_eval "cases { }"                                                             [ "forall a. either {  } -> a" ];
  test_eval "c = cases { A(x) -> x+1, B(x) -> x*x, C(_) -> 0 }, case B(2) of (c)"   [ "c : forall a. either { A(Int), B(Int), C(a) } -> Int"; "4" ];
  test_eval "case A(1) of { A(x) -> \"first\", A(x) -> \"second\" }"                [ "\"first\"" ];
  test_eval "show_sign = cases { Pos(x) -> \"positive\", Neg(x) -> \"negative\" }
             sign x = if x >= 0 then Pos(x) else Neg(x)
             case (sign 3) of (show_sign)
             case (sign (-3)) of (show_sign)"                                       [ "show_sign : forall a, b. either { Pos(b), Neg(a) } -> String";
                                                                                      "sign : forall a union, b. Int -> either { Neg(Int), Pos(Int) } or a";
                                                                                      "\"positive\""; "\"negative\"" ];
(*
  test_eval "sum = monoid 0 (+), count = mapping (x -> 1) sum
        (reduce (group_reducer (x -> x mod 3) (x -> x) count) (range 1 10)).pairs"  [ "sum : Reducer(Int, Int, Int)"; "count : forall a. Reducer(a, Int, Int)";
                                                                                      "[(2, 3), (0, 3), (1, 4)]" ];
  test_eval "sum = monoid 0 (+)
             set = project (.keys) (group_reducer (x -> x) (x -> 1) sum)
             partition p = reduce (group_reducer p (x -> x) set)
             (partition (x -> x mod 3) (range 1 10)).pairs"                         [ "sum : Reducer(Int, Int, Int)";
                                                                                      "set : forall a. Reducer(a, [a])";
                                                                                      "partition : forall a, b. (a -> b) -> [a] -> { keys:[b], pairs:[(b, [a])], values:b -> [[a]] }";
                                                                                      "[(2, [2, 8, 5]), (0, [6, 9, 3]), (1, [1, 4, 7, 10])]" ];
*)
(*
  test_eval "reduce (top fst snd 2) [(1,2),(4,9),(1,3),(8,19)]"                     [ "[(8, 19), (4, 9)]" ];
  test_eval "reduce (top fst snd 2) [(1,2)]"                                        [ "[(1,2)]" ];
  test_eval "reduce (top fst snd 2) [(1,2),(1,3)]"                                  [ "[(1,3)]" ];
  test_eval "reduce (top fst snd 2) [(1,3),(1,2)]"                                  [ "[(1,3)]" ];
  test_eval "reduce (top fst snd 2) [(1,3),(2,1),(1,2)]"                            [ "[(1,3),(2,1)]" ];
*)
(*
  test_eval "insert (k,v) = 1
             remove (k,v) = -1
             a = \"a\", b = \"b\", c = \"c\", d = \"d\"
             count_updates = group_updates fst snd (last_or \"\") insert remove
             reduce (monoid 0 (+)) (count_updates [(a,d),(b,d),(a,c),(c,a),(b,b)])" [ "insert : forall a, b. (a, b) -> Int";
                                                                                      "remove : forall a, b. (a, b) -> Int";
                                                                                      "a = \"a\""; "b = \"b\""; "c = \"c\""; "d = \"d\"";
                                                                                      "count_updates : forall a. [(a, String)] -> [Int]";
                                                                                      "3" ];
  test_eval "insert (k,v) = (v,1)
             remove (k,v) = (v,-1)
             a = \"a\", b = \"b\", c = \"c\", d = \"d\"
             value_count_updates = group_updates fst snd (last_or \"\") insert remove
             value_count_updates [(a,d),(b,d),(a,c),(c,a),(b,b)]"                   [ "insert : forall a, b. (a, b) -> (b, Int)";
                                                                                      "remove : forall a, b. (a, b) -> (b, Int)";
                                                                                      "a = \"a\""; "b = \"b\""; "c = \"c\""; "d = \"d\"";
                                                                                      "value_count_updates : forall a. [(a, String)] -> [(String, Int)]";
                                                                                      "[(\"d\", 1), (\"d\", 1), (\"d\", -1), (\"c\", 1), (\"a\", 1), (\"d\", -1), (\"b\", 1)]" ];
*)

  test_format "json"   ":Int"                                                       "123";
  test_format "json"   ":String"                                                    "\"xoxox\"";
  test_format "json"   ":Bool"                                                      "false";
  test_format "json"   ":()"                                                        "()";
  test_format "json"   ":(Int,String,Bool)"                                         "(123, \"foo\", true)";
  test_format "json"   ":{ x:Int, y:String, z:Bool }"                               "{ x = 123, y = \"foo\", z=true }";
  test_format "json"   ":([Int],String,Bool)"                                       "([1,2,3], \"foo\", true)";
  test_format "json"   ":(Int,[String])"                                            "(123, [\"foo\", \"bar\"])";
  test_format "json"   ":([Int],[String])"                                          "([1,2,3], [\"foo\", \"bar\"])";
  test_format "json"   ":{ x:[Int], y:String, z:Bool }"                             "{ x = [1,2,3], y = \"foo\", z=true }";
  test_format "json"   ":[[Int]]"                                                   "[[],[1],[1,2,3]]";
  test_format "json"   ":[{ x:Int, y:String, z:Bool }]"                             "[{ x = 123, y = \"foo\", z=true }, { x = 321, y = \"bar\", z = false}]";
  test_format "json"   ":{ xs: [[Int]], y: { x:[Int], y:String, z:Bool }}"          "{ xs = [[],[1],[1,2,3]], y = { x = [1,2,3], y = \"foo\", z = true }}";

  test_format "marshal"   ":Int"                                                    "123";
  test_format "marshal"   ":String"                                                 "\"xoxox\"";
  test_format "marshal"   ":Bool"                                                   "false";
  test_format "marshal"   ":()"                                                     "()";
  test_format "marshal"   ":(Int,String,Bool)"                                      "(123, \"foo\", true)";
  test_format "marshal"   ":{ x:Int, y:String, z:Bool }"                            "{ x = 123, y = \"foo\", z=true }";
  test_format "marshal"   ":([Int],String,Bool)"                                    "([1,2,3], \"foo\", true)";
  test_format "marshal"   ":(Int,[String])"                                         "(123, [\"foo\", \"bar\"])";
  test_format "marshal"   ":([Int],[String])"                                       "([1,2,3], [\"foo\", \"bar\"])";
  test_format "marshal"   ":{ x:[Int], y:String, z:Bool }"                          "{ x = [1,2,3], y = \"foo\", z=true }";
  test_format "marshal"   ":[[Int]]"                                                "[[],[1],[1,2,3]]";
  test_format "marshal"   ":[{ x:Int, y:String, z:Bool }]"                          "[{ x = 123, y = \"foo\", z=true }, { x = 321, y = \"bar\", z = false}]";
  test_format "marshal"   ":{ xs: [[Int]], y: { x:[Int], y:String, z:Bool }}"       "{ xs = [[],[1],[1,2,3]], y = { x = [1,2,3], y = \"foo\", z = true }}";

  test_error "x"                                                                    "ERROR: unknown value 'x'";
  test_error "add true"                                                             "TYPE ERROR: incompatible types 'Int' and 'Bool'";
  test_error "f x = x(x)"                                                           "TYPE ERROR: incompatible types 'a' and 'a -> b'";
  test_error "{ x = 1 }.foo"                                                        "TYPE ERROR: missing fields { foo:a }";
  test_error "g r = add (r.x) (r.y), g {}"                                          "TYPE ERROR: missing fields { x:Int, y:Int }";
  test_error "g r = add (r.x) (r.y), g { y = 2 }"                                   "TYPE ERROR: missing fields { x:Int }";
  test_error "g r = add (r.x) (r.y), g g"                                           "TYPE ERROR: incompatible types 'a -> Int' and '{ x:Int, y:Int }'";
  test_error "seq + (1,2,3,\"foo\",6,7)"                                            "TYPE ERROR: incompatible types 'Int' and 'String'";
  test_error ":Foo"                                                                 "TYPE ERROR: unknown type 'Foo'";
  test_error "cases { A(x) -> x + 1, B(x) -> x * x, C(x) -> true }"                 "TYPE ERROR: incompatible types 'Int' and 'Bool'";
]
