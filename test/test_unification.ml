open OUnit2
open Schemer.Types
open Schemer.Unification

let list_to_constraints xs =
  List.fold_left (fun c (t1, t2) -> Constraint.add c t1 t2) Constraint.empty xs
;;

let uni_make_eq n constrs e =
  n >:: fun _ -> assert_equal (Substitution.to_list (unify (list_to_constraints constrs))) e
;;

let uni_make_ex n constrs e =
  n >:: fun _ -> assert_raises e (fun () -> unify (list_to_constraints constrs))
;;

let uni_suite =
  "unification"
  >::: [ uni_make_eq "1" [(TVar "x", TInt)] [("x", TInt)]
       ; uni_make_ex "2" [(TVar "x", (TFun (TVar "x", TInt)))] Unification_failed
       ]
;;

let () = run_test_tt_main uni_suite
