open OUnit2
open Series
open Util

let test_apply name f x y =
  let test test_ctxt =
    assert_equal y (f x)
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
]

