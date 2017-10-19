open OUnit2
open Series
open Util

let seq = Bounded.of_list

let test_apply name f x y =
  let test test_ctxt =
    assert_equal y (f x)
  in name >:: test

let test_apply2 name f x y z =
  let test test_ctxt =
    assert_equal z (f x y)
  in name >:: test

let test_reducer name f =
  test_apply name (Bounded.of_list >> f)

let test_transducer name f =
  test_apply name (Bounded.of_list >> f >> Bounded.to_list)

let gt x = fun y -> y > x
let sum = Reducer.commutative_monoid 0 (+)

let suite = "series">:::[
  test_transducer "map" (Bounded.map succ) [1;2;3] [2;3;4];
  test_transducer "filter" (Bounded.filter (gt 0)) [-1;-2;3] [3];
  test_reducer "sum" (Bounded.reduce sum) [1;2;3;4] 10;
  test_transducer "unique" (Bounded.unique) [1;2;2;3;1;4] [1;2;3;4];
  test_transducer "unique" (Bounded.unique) [1;4;2;3;1;2] [1;4;2;3];
  test_transducer "unique" (Bounded.unique) [] [];
  test_reducer "string reducer" (Bounded.show "(" ", " ")" string_of_int) [1;2;3;4] "(1, 2, 3, 4)";
  test_reducer "string reducer" (Bounded.show "(" ", " ")" string_of_int) [] "()";
  test_apply2 "series equality" (Bounded.equal (==)) (seq [1;2;3]) (seq [1;2;3]) true;
  test_apply2 "series equality" (Bounded.equal (==)) (seq [1;2;3;4]) (seq [1;2;3]) false;
  test_apply2 "series equality" (Bounded.equal (==)) (seq [1;2;3]) (seq [1;2;3;4]) false;
  test_apply2 "series equality" (Bounded.equal (==)) (seq [1;2;3]) (seq [3;2;1]) false;
]

