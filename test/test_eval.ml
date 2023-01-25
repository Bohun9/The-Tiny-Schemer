open OUnit2
open Schemer.Values

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let parse_make n f e =
  n >:: fun _ -> assert_equal (Schemer.Eval.eval_dynamic (Schemer.Interp.parse (read_whole_file f))) e
;;

let fib_val = VInt 55
let id_val = VInt 0

let type_suite =
  "type inference"
  >::: [ parse_make "1" "code/fib" fib_val
       ; parse_make "2" "code/id" id_val
       ]
;;

let () = run_test_tt_main type_suite
