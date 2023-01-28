open OUnit2
open Schemer.Types

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

module M = Map.Make (String)

let check_isomorphism t1 t2 =
  let res = ref true in
  let map0 = ref M.empty in
  let map1 = ref M.empty in
  let add m k v =
    (match M.find_opt k !m with
     | None -> ()
     | Some w -> if v == w then () else res := false);
    m := M.add k v !m
  in
  let rec run t1 t2 =
    match t1, t2 with
    | TInt, TInt -> ()
    | TBool, TBool -> ()
    | TUnit, TUnit -> ()
    | TError, TError -> ()
    | TList t1, TList t2 -> run t1 t2
    | TCont t1, TCont t2 -> run t1 t2
    | TRef t1, TRef t2 -> run t1 t2
    | TPair (t11, t12), TPair (t21, t22) ->
      run t11 t21;
      run t12 t22
    | TFun (t11, t12), TFun (t21, t22) ->
      run t11 t21;
      run t12 t22
    | TVar x, TVar y ->
      add map0 x y;
      add map1 y x
    | _ -> res := false
  in
  run t1 t2;
  !res
;;

let type_make n f e =
  n
  >:: fun _ ->
  assert_bool
    "what is this"
    (check_isomorphism
       (Schemer.Typecheck.typeof (Schemer.Interp.parse (read_whole_file f)))
       e)
;;

let type_make_ex n f =
  n
  >:: fun _ ->
  assert_raises Schemer.Unification.Unification_failed (fun _ ->
    Schemer.Typecheck.typeof (Schemer.Interp.parse (read_whole_file f)))
;;

let fib_type = TInt
let id_type = TInt

let typeinfer_type =
  TFun
    ( TFun (TVar "x", TFun (TVar "y", TVar "z"))
    , TFun (TFun (TVar "x", TVar "y"), TFun (TVar "x", TVar "z")) )
;;

let list_length_type = TFun (TList (TVar "x"), TInt)

let type_suite =
  "type inference"
  >::: [ type_make "1" "code/fib" fib_type
       ; type_make "2" "code/id" id_type
       ; type_make "3" "code/typeinfer" typeinfer_type
       ; type_make_ex "4" "code/ycomb"
       ; type_make "5" "code/list_length" list_length_type
       ]
;;

let () = run_test_tt_main type_suite
