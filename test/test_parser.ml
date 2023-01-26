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

let id_ast =
  Let
    ( "id"
    , Lam ("x", Id "x")
    , If (App (Id "id", Bool true), App (Id "id", Int 0), App (Id "id", Int 1)) )
;;

let mut_ast =
  Let
    ( "x"
    , Ref (Int 0)
    , Begin (Set (Id "x", Binop (Add, Deref (Id "x"), Int 1)), Deref (Id "x")) )
;;

let cont_ast = Callcc ("k", Binop (Add, Int 1, Throw (Id "k", Int 2)))

let parser_suite =
  "parsing"
  >::: [ parse_make "1" "code/fib" fib_ast
       ; parse_make "2" "code/id" id_ast
       ; parse_make "3" "code/mutation" mut_ast
       ; parse_make "4" "code/cont" cont_ast
       ]
;;

let () = run_test_tt_main parser_suite
