open OUnit2
open Schemer.Ast

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let parse_make n f e =
  n >:: fun _ -> assert_equal (Schemer.Interp.parse (read_whole_file f)) e
;;

let fib_ast =
  Letrec
    ( "fib"
    , "n"
    , If
        ( Binop (Leq, Id "n", Int 1)
        , Id "n"
        , Binop
            ( Add
            , App (Id "fib", Binop (Sub, Id "n", Int 1))
            , App (Id "fib", Binop (Sub, Id "n", Int 2)) ) )
    , App (Id "fib", Int 10) )
;;

let parser_suite =
  "parsing"
  >::: [ parse_make "1" "code/fib" fib_ast
       ; parse_make "1" "code/fib" fib_ast
       ; parse_make "1" "code/fib" fib_ast
       ]
;;

let () = run_test_tt_main parser_suite
