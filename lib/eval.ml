open Ast
open Values

exception IfGuardTypeError
exception AddTypeError
exception SubTypeError
exception MultTypeError
exception LeqTypeError
exception GeqTypeError
exception EqTypeError
exception DereferenceTypeError
exception AssignmentTypeError
exception ContinuationTypeError
exception FunctionApplicationTypeError
exception PairFirstTypeError
exception PairSecondTypeError
exception EmptyTypeError
exception CarTypeError
exception CdrTypeError
exception ConsError
exception ConsTypeError
exception CarEmptyError
exception CdrEmptyError

let evalbinop (op : binop) (v1 : value) (v2 : value) =
  match op with
  | Add ->
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 + i2)
     | _ -> raise AddTypeError)
  | Sub ->
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> raise SubTypeError)
  | Mult ->
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> raise MultTypeError)
  | Leq ->
    (match v1, v2 with
     | VInt i1, VInt i2 -> VBool (i1 <= i2)
     | _ -> raise LeqTypeError)
  | Geq ->
    (match v1, v2 with
     | VInt i1, VInt i2 -> VBool (i1 >= i2)
     | _ -> raise GeqTypeError)
  | Eq ->
    (match v1, v2 with
     | VInt i1, VInt i2 -> VBool (i1 == i2)
     | _ -> raise EqTypeError)
;;

let rec eval
  (e : expr)
  (env : value Environment.t)
  (sto : value Store.t)
  (k : result -> result)
  : result
  =
  match e with
  | Int x -> k (VInt x, sto)
  | Bool x -> k (VBool x, sto)
  | Unit -> k (VUnit, sto)
  | Error -> VError, sto
  | Id x ->
    (* (let _ = Printf.printf "%s %s\n" x (to_string (Environment.query env x)) in ()); *)
    k (Environment.query env x, sto)
  | Lam (x, b) -> k (VClo (x, b, env), sto)
  | App (f, e) ->
    eval f env sto (fun (f, sto) ->
      eval e env sto (fun (v, sto) ->
        (* (let _ = Printf.printf "f -- %s\n" (to_string f) in *)
        (*  ()); *)
        (* (let _ = Printf.printf "v -- %s\n" (to_string v) in *)
        (*  ()); *)
        match f with
        | VClo (x, b, clo_env) ->
          eval b (clo_env || (x, v)) sto (fun (v, sto) -> k (v, sto))
        | VCloRec (fn, x, b, clo_env) ->
          eval b ((clo_env || (x, v)) || (fn, f)) sto (fun (v, sto) -> k (v, sto))
        | _ -> raise FunctionApplicationTypeError))
  | Pair (e1, e2) ->
    eval e1 env sto (fun (v1, sto) ->
      eval e2 env sto (fun (v2, sto) -> k (VPair (v1, v2), sto)))
  | First e ->
    eval e env sto (fun (v, sto) ->
      match v with
      | VPair (v1, _) -> k (v1, sto)
      | _ -> raise PairFirstTypeError)
  | Second e ->
    eval e env sto (fun (v, sto) ->
      match v with
      | VPair (_, v2) -> k (v2, sto)
      | _ -> raise PairSecondTypeError)
  | Let (x, e1, e2) ->
    eval e1 env sto (fun (v1, sto) ->
      eval e2 (env || (x, v1)) sto (fun (v2, sto) -> k (v2, sto)))
  | If (e1, e2, e3) ->
    eval e1 env sto (fun (v, sto) ->
      match v with
      | VBool true -> eval e2 env sto (fun (v, sto) -> k (v, sto))
      | VBool false ->
        eval e3 env sto (fun (v, sto) ->
          (* (let _ = Printf.printf "IF FALSE: %s\n" (to_string v) in ()); *)
          k (v, sto))
      | _ -> raise IfGuardTypeError)
  | Binop (op, e1, e2) ->
    eval e1 env sto (fun (v1, sto) ->
      eval e2 env sto (fun (v2, sto) -> k (evalbinop op v1 v2, sto)))
  | Nil -> k (VNil, sto)
  | Empty e ->
    eval e env sto (fun (v, sto) ->
      match v with
      | VNil -> k (VBool true, sto)
      | VCons (_, _) -> k (VBool false, sto)
      | _ -> raise EmptyTypeError)
  | Car e ->
    eval e env sto (fun (v, sto) ->
      match v with
      | VNil -> raise CarEmptyError
      | VCons (h, _) -> k (h, sto)
      | _ -> raise CarTypeError)
  | Cdr e ->
    eval e env sto (fun (v, sto) ->
      match v with
      | VNil -> raise CdrEmptyError
      | VCons (_, t) -> k (t, sto)
      | _ -> raise CdrTypeError)
  | Cons (e1, e2) ->
    eval e1 env sto (fun (v1, sto) ->
      eval e2 env sto (fun (v2, sto) ->
        match v2 with
        | VNil | VCons (_, _) -> k (VCons (v1, v2), sto)
        | _ -> raise ConsTypeError))
  | Letrec (f, x, b, e) ->
    let env = env || (f, VCloRec (f, x, b, env)) in
    eval e env sto (fun (v, sto) -> k (v, sto))
  | Ref e ->
    eval e env sto (fun (v, sto) ->
      let sto, loc = Store.add sto v in
      k (VLoc loc, sto))
  | Deref e ->
    eval e env sto (fun (v, sto) ->
      match v with
      | VLoc loc ->
        let v = Store.query sto loc in
        k (v, sto)
      | _ -> raise DereferenceTypeError)
  | Set (e1, e2) ->
    eval e1 env sto (fun (v1, sto) ->
      eval e2 env sto (fun (v2, sto) ->
        match v1 with
        | VLoc loc -> k (VUnit, Store.set sto loc v2)
        | _ -> raise AssignmentTypeError))
  | Begin (e1, e2) ->
    eval e1 env sto (fun (_, sto) -> eval e2 env sto (fun (v2, sto) -> k (v2, sto)))
  | Callcc (kn, e) -> eval e (env || (kn, VCont k)) sto (fun (v, sto) -> k (v, sto))
  | Throw (k2, e) ->
    eval k2 env sto (fun (k2, sto) ->
      eval e env sto (fun (v, sto) ->
        match k2 with
        | VCont k2 -> k2 (v, sto)
        | _ -> raise ContinuationTypeError))
  | IsCont e ->
    eval e env sto (fun (v, sto) ->
      match v with
      | VCont _ -> k (VBool true, sto)
      | _ -> k (VBool false, sto))
;;

let evaluate (e : expr) : value =
  let v, _ = eval e Environment.empty Store.empty (fun x -> x) in
  v
;;
