open Ast
open Values

exception IfGuardTypeError
exception AddTypeError
exception SubTypeError
exception MultTypeError
exception LeqTypeError
exception GeqTypeError
exception DereferenceTypeError
exception AssignmentTypeError
exception ContinuationTypeError
exception FunctionApplicationTypeError

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
  | Id x -> k (Environment.query env x, sto)
  | Lam (x, b) -> k (VClo (x, b, env), sto)
  | App (f, e) ->
    eval f env sto (fun (f, sto) ->
      eval e env sto (fun (v, sto) ->
        match f with
        | VClo (x, b, clo_env) ->
          eval b (Environment.extend clo_env x v) sto (fun (v, sto) -> k (v, sto))
        | VCloRec (fn, x, b, clo_env) ->
          eval
            b
            (Environment.extend (Environment.extend clo_env x v) fn f)
            sto
            (fun (v, sto) -> k (v, sto))
        | _ -> raise FunctionApplicationTypeError))
  | Let (x, e1, e2) ->
    eval e1 env sto (fun (v1, sto) ->
      eval e2 (Environment.extend env x v1) sto (fun (v2, sto) -> k (v2, sto)))
  | If (e1, e2, e3) ->
    eval e1 env sto (fun (v, sto) ->
      match v with
      | VBool true -> eval e2 env sto (fun (v, sto) -> k (v, sto))
      | VBool false -> eval e3 env sto (fun (v, sto) -> k (v, sto))
      | _ -> raise IfGuardTypeError)
  | Binop (op, e1, e2) ->
    eval e1 env sto (fun (v1, sto) ->
      eval e2 env sto (fun (v2, sto) -> k (evalbinop op v1 v2, sto)))
  | Letrec (f, x, b, e) ->
    let env = Environment.extend env f (VCloRec (f, x, b, env)) in
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
  | Callcc (kn, e) ->
    eval e (Environment.extend env kn (VCont k)) sto (fun (v, sto) -> k (v, sto))
  | Throw (k2, e) ->
    eval k2 env sto (fun (k2, sto) ->
      eval e env sto (fun (v, sto) ->
        match k2 with
        | VCont k2 -> k2 (v, sto)
        | _ -> raise ContinuationTypeError))
;;

let eval_dynamic (e : expr) =
  let v, _ = eval e Environment.empty Store.empty (fun x -> x) in
  v
;;

let eval_static (e : expr) =
  let _ = Typecheck.typeof e in
  e |> eval_dynamic
;;
