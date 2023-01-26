open OUnit2
open Schemer.Types

let ftv_make n t e = n >:: fun _ -> assert_equal (free_type_variables t) e

let ftv_suite =
  "free_type_variables"
  >::: [ ftv_make "1" (TVar "x") [ "x" ]
       ; ftv_make "2" (TFun (TVar "x", TVar "y")) [ "x"; "y" ]
       ; ftv_make "3" (TFun (TVar "x", TVar "x")) [ "x" ]
       ; ftv_make "4" (TFun (TBool, TInt)) []
       ]
;;

let () = run_test_tt_main ftv_suite

let list_to_subst xs =
  List.fold_left (fun s (x, t) -> s $$ (x >> t)) Substitution.empty xs
;;

let subst_make n subst t1 t2 =
  n >:: fun _ -> assert_equal (Substitution.apply (list_to_subst subst) t1) t2
;;

let subst_suite =
  "substitution"
  >::: [ subst_make "1" [ "x", TVar "y" ] (TVar "x") (TVar "y")
       ; subst_make "2" [ "y", TVar "z"; "x", TVar "y" ] (TVar "x") (TVar "z")
       ; subst_make "3" [ "x", TVar "y"; "y", TVar "z" ] (TVar "x") (TVar "y")
       ]
;;

let () = run_test_tt_main subst_suite
