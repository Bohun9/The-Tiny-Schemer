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
  assert_equal (Schemer.Eval.eval_dynamic (Schemer.Interp.parse (read_whole_file f))) e
;;

let eval_make_sta n f e =
  n
  >:: fun _ ->
  assert_equal (Schemer.Eval.eval_static (Schemer.Interp.parse (read_whole_file f))) e
;;

let fib_val = VInt 55
let id_val = VInt 0
let mut_val = VInt 1

let eval_suite =
  "evaluation"
  >::: [ eval_make_dyn "1" "code/fib" fib_val
       ; eval_make_dyn "2" "code/id" id_val
       ; eval_make_dyn "3" "code/mutation" mut_val
       ; eval_make_sta "4" "code/fib" fib_val
       ; eval_make_sta "5" "code/id" id_val
       ; eval_make_sta "6" "code/mutation" mut_val
       ]
;;

let () = run_test_tt_main eval_suite
