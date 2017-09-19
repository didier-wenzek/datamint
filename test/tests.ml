open OUnit2

let suite = "datamint tests">:::[
  Test_series.suite
]

let () =
  run_test_tt_main suite
