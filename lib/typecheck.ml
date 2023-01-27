open Types
open Unification

exception IndenNotInScope_TypeContext of string

let genvar =
  let r = ref 0 in
  let g () =
    r := !r + 1;
    TVar ("t" ^ string_of_int !r)
  in
  g
;;

module TypeContext : sig
  type t

  val empty : t
  val ( || ) : t -> string * tscheme -> t
  val apply : t -> Types.Substitution.t -> t
  val query : t -> string -> ttype
  val free_type_variables : t -> string list
  val to_list : t -> (string * tscheme) list
end = struct
  type t = (string * tscheme) list

  let empty = []
  let ( || ) cxt b = b :: cxt

  let apply cxt subst =
    List.map (fun (x, Forall (pv, t)) -> x, Forall (pv, Substitution.apply subst t)) cxt
  ;;

  let query (ctx : t) (x : string) : ttype =
    match List.assoc_opt x ctx with
    | Some (Forall (pv, t)) ->
      Substitution.apply
        (List.fold_left (fun s x -> s $$ (x >> genvar ())) Substitution.empty pv)
        t
    | _ -> raise (IndenNotInScope_TypeContext x)
  ;;

  let free_type_variables (ctx : t) : string list =
    List.fold_left
      (fun v (_, Forall (pv, t)) ->
        Utils.unite_lists
          v
          (List.filter (fun x -> not (List.mem x pv)) (free_type_variables t)))
      []
      ctx
  ;;

  let to_list (ctx : t) : (string * tscheme) list = ctx
end

let ( || ) = TypeContext.( || )

let generalize (ctx : TypeContext.t) (t : ttype) (c : Constraint.t list) : tscheme =
  let subst = Unification.unify c in
  let t = Substitution.apply subst t in
  let ctx = TypeContext.apply ctx subst in
  Forall
    ( Utils.difference_lists (free_type_variables t) (TypeContext.free_type_variables ctx)
    , t )
;;

let rec infer (e : Ast.expr) (ctx : TypeContext.t) : ttype * Unification.Constraint.t list
  =
  match e with
  | Unit -> TUnit, Constraint.empty
  | Int _ -> TInt, Constraint.empty
  | Bool _ -> TBool, Constraint.empty
  | Id x -> TypeContext.query ctx x, Constraint.empty
  | Lam (x, b) ->
    let at = genvar () in
    let bt, c = infer b (ctx || (x, Forall ([], at))) in
    TFun (at, bt), c
  | App (f, a) ->
    let ft, c1 = infer f ctx in
    let at, c2 = infer a ctx in
    let rt = genvar () in
    rt, c1 ++ c2 ++ (ft == TFun (at, rt))
  | Pair (e1, e2) ->
    let t1, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    TPair (t1, t2), c1 ++ c2
  | First e ->
    let t, c = infer e ctx in
    let t1 = genvar () in
    let t2 = genvar () in
    t1, c ++ (t == TPair (t1, t2))
  | Second e ->
    let t, c = infer e ctx in
    let t1 = genvar () in
    let t2 = genvar () in
    t2, c ++ (t == TPair (t1, t2))
  | Let (x, e1, e2) ->
    let t1, c1 = infer e1 ctx in
    (match e1 with
     | Unit | Int _ | Bool _ | Id _ | Lam (_, _) ->
       let t2, c2 = infer e2 (ctx || (x, generalize ctx t1 c1)) in
       t2, c1 ++ c2
     | _ ->
       let t2, c2 = infer e2 (ctx || (x, Forall ([], t1))) in
       t2, c1 ++ c2)
  | If (e1, e2, e3) ->
    let t1, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    let t3, c3 = infer e3 ctx in
    t2, c1 ++ c2 ++ c3 ++ (t1 == TBool) ++ (t2 == t3)
  | Binop (op, e1, e2) ->
    let t1, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    (match op with
     | Add | Sub | Mult -> TInt, c1 ++ c2 ++ (t1 == TInt) ++ (t2 == TInt)
     | Leq | Geq -> TBool, c1 ++ c2 ++ (t1 == TInt) ++ (t2 == TInt))
  | Nil ->
    let t = genvar () in
    TList t, Constraint.empty
  | Empty e ->
    let t, c = infer e ctx in
    let te = genvar () in
    TBool, c ++ (TList te == t)
  | Car e ->
    let t, c = infer e ctx in
    let te = genvar () in
    te, c ++ (TList te == t)
  | Cdr e ->
    let t, c = infer e ctx in
    let te = genvar () in
    t, c ++ (TList te == t)
  | Cons (e1, e2) ->
    let t1, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    t2, c1 ++ c2 ++ (t2 == TList t1)
  | Letrec (f, x, b, e) ->
    let ft = genvar () in
    let xt = genvar () in
    let t1, c1 = infer b ((ctx || (f, Forall ([], ft))) || (x, Forall ([], xt))) in
    let t2, c2 = infer e (ctx || (f, Forall ([], ft))) in
    t2, c1 ++ c2 ++ (ft == TFun (xt, t1))
  | Ref x ->
    let t, c = infer x ctx in
    TRef t, c
  | Deref x ->
    let t, c = infer x ctx in
    let rt = genvar () in
    rt, c ++ (t == TRef rt)
  | Set (e1, e2) ->
    let t1, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    TUnit, c1 ++ c2 ++ (t1 == TRef t2)
  | Begin (e1, e2) ->
    let _, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    t2, c1 ++ c2
  | Callcc (k, b) ->
    let kt = genvar () in
    let t, c = infer b (ctx || (k, Forall ([], TCont kt))) in
    t, c
  | Throw (k, x) ->
    let kt, c1 = infer k ctx in
    let xt, c2 = infer x ctx in
    let rt = genvar () in
    rt, c1 ++ c2 ++ (kt == TCont xt)
;;

let typeof (e : Ast.expr) : ttype =
  let t, c = infer e TypeContext.empty in
  let subst = Unification.unify c in
  Substitution.apply subst t
;;

let substitutionof (e : Ast.expr) : Substitution.t =
  let _, c = infer e TypeContext.empty in
  let subst = Unification.unify c in
  subst
;;

let constraintsof (e : Ast.expr) : Constraint.t list =
  let _, c = infer e TypeContext.empty in
  c
;;
