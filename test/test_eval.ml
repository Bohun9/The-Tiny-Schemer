open OUnit2
open Schemer.Values

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let eval_make_dyn n f e =
  n
  >:: fun _ ->
  assert_equal (Schemer.Interp.run_dynamic (read_whole_file f)) e
;;

let eval_make_sta n f e =
  n
  >:: fun _ ->
  assert_equal (Schemer.Interp.run_static (read_whole_file f)) e
;;

let fib_val = VInt 55
let id_val = VInt 0
let mut_val = VInt 1
let cont_val = VInt 2
let fact_iter_val = VInt 3628800
let fact_knot_val = VInt 3628800

let eval_suite =
  "evaluation"
  >::: [ eval_make_dyn "1" "code/fib" fib_val
       ; eval_make_dyn "2" "code/id" id_val
       ; eval_make_dyn "3" "code/mutation" mut_val
       ; eval_make_sta "4" "code/fib" fib_val
       ; eval_make_sta "5" "code/id" id_val
       ; eval_make_sta "6" "code/mutation" mut_val
       ; eval_make_dyn "7" "code/cont" cont_val
       ; eval_make_sta "8" "code/cont" cont_val
       ; eval_make_dyn "9" "code/fact_iter" fact_iter_val
       ; eval_make_dyn "9" "code/fact_knot" fact_knot_val
       ; eval_make_sta "10" "code/fact_knot" fact_knot_val
       ]
;;

let () = run_test_tt_main eval_suite
