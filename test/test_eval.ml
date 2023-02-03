open OUnit2
open Schemer.Values

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let eval_make_dyn n f e =
  n >:: fun _ -> assert_equal (Schemer.Interp.run_dynamic (read_whole_file f)) e
;;

let eval_make_sta n f e =
  n >:: fun _ -> assert_equal (Schemer.Interp.run_static (read_whole_file f)) e
;;

let fib_val = VInt 55
let id_val = VInt 0
let mut_val = VInt 1
let cont_val = VInt 2
let fact_iter_val = VInt 3628800
let fact_knot_val = VInt 3628800
let pair_val = VInt 3
let list_val = VInt 3
let error_val = VError
let pytha_val = VPair (VInt 8, VPair (VInt 6, VInt 10))
let exn_val = VPair (VInt 0, VInt 1)
let poly_list_length_val = VInt 6

let coroutines_val =
  VCons
    ( VInt 0
    , VCons
        ( VInt 1
        , VCons
            ( VInt 2
            , VCons
                ( VInt 3
                , VCons
                    ( VInt 0
                    , VCons
                        ( VInt 1
                        , VCons
                            ( VInt 2
                            , VCons
                                ( VInt 3
                                , VCons
                                    ( VInt 0
                                    , VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNil)))
                                    ) ) ) ) ) ) ) ) )
;;

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
       ; eval_make_sta "11" "code/pair" pair_val
       ; eval_make_dyn "12" "code/list" list_val
       ; eval_make_sta "13" "code/list" list_val
       ; eval_make_sta "14" "code/error" error_val
       ; eval_make_dyn "15" "code/pythagorean" pytha_val
       ; eval_make_dyn "16" "code/exception" exn_val
       ; eval_make_sta "17" "code/poly_list_length" poly_list_length_val
       ; eval_make_dyn "18" "code/coroutines" coroutines_val
       ]
;;

let () = run_test_tt_main eval_suite
