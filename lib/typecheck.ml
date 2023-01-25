open Types
open Unification

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
  val extend : t -> string -> string list -> ttype -> t
  val apply : t -> Types.Substitution.t -> t
  val query : t -> string -> ttype
  val free_type_variables : t -> string list
  val to_list : t -> (string * string list * ttype) list
end = struct
  type t = (string * string list * ttype) list

  let empty = []
  let extend cxt x pv t = (x, pv, t) :: cxt
  let apply cxt subst = List.map (fun (x, pv, t) -> x, pv, Substitution.apply subst t) cxt

  let rec query (ctx : t) (x : string) : ttype =
    match ctx with
    | [] -> failwith "variable not found in context"
    | (y, pv, t) :: ctx ->
      if x = y
      then
        Substitution.apply
          (List.fold_left
             (fun s x -> Substitution.compose s x (genvar ()))
             Substitution.empty
             pv)
          t
      else query ctx x
  ;;

  let free_type_variables (ctx : t) : string list =
    List.fold_left
      (fun v (_, p, t) ->
        Utils.unite_lists
          v
          (List.filter (fun x -> not (List.mem x p)) (free_type_variables t)))
      []
      ctx
  ;;

  let to_list (ctx : t) : (string * string list * ttype) list = ctx
end

let rec infer (e : Ast.expr) (ctx : TypeContext.t) : ttype * Unification.Constraint.t list
  =
  match e with
  | Unit -> TUnit, Constraint.empty
  | Int _ -> TInt, Constraint.empty
  | Bool _ -> TBool, Constraint.empty
  | Id x -> TypeContext.query ctx x, Constraint.empty
  | Lam (x, b) ->
    let at = genvar () in
    let bt, c = infer b (TypeContext.extend ctx x [] at) in
    TFun (at, bt), c
  | App (f, a) ->
    let ft, c1 = infer f ctx in
    let at, c2 = infer a ctx in
    let rt = genvar () in
    let c = Constraint.unite c1 c2 in
    rt, Constraint.add c ft (TFun (at, rt))
  | Let (x, e1, e2) ->
    let t1, c1 = infer e1 ctx in
    (match e1 with
     | Unit | Int _ | Bool _ | Id _ | Lam (_, _) ->
       let subst = Unification.unify c1 in
       let t1 = Substitution.apply subst t1 in
       let ctx = TypeContext.apply ctx subst in
       let ctx =
         TypeContext.extend
           ctx
           x
           (Utils.difference_lists
              (free_type_variables t1)
              (TypeContext.free_type_variables ctx))
           t1
       in
       let t2, c2 = infer e2 ctx in
       let c = Constraint.unite c1 c2 in
       t2, c
     | _ ->
       let t2, c2 = infer e2 (TypeContext.extend ctx x [] t1) in
       let c = Constraint.unite c1 c2 in
       t2, c)
  | If (e1, e2, e3) ->
    let t1, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    let t3, c3 = infer e3 ctx in
    let c = Constraint.unite (Constraint.unite c1 c2) c3 in
    t2, Constraint.add (Constraint.add c t1 TBool) t2 t3
  | Binop (op, e1, e2) ->
    let t1, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    let c = Constraint.unite c1 c2 in
    (match op with
     | Add | Sub | Mult -> TInt, Constraint.add (Constraint.add c t1 TInt) t2 TInt
     | Leq | Geq -> TBool, Constraint.add (Constraint.add c t1 TInt) t2 TInt)
  | Letrec (f, x, b, e) ->
    let ft = genvar () in
    let xt = genvar () in
    let bctx = TypeContext.extend (TypeContext.extend ctx f [] ft) x [] xt in
    let ectx = TypeContext.extend ctx f [] ft in
    let t1, c1 = infer b bctx in
    let t2, c2 = infer e ectx in
    let c = Constraint.unite c1 c2 in
    t2, Constraint.add c ft (TFun (xt, t1))
  | Ref x ->
    let t, c = infer x ctx in
    TRef t, c
  | Deref x ->
    let t, c = infer x ctx in
    let rt = genvar () in
    rt, Constraint.add c t (TRef rt)
  | Set (e1, e2) ->
    let t1, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    let c = Constraint.unite c1 c2 in
    TUnit, Constraint.add c t1 (TRef t2)
  | Begin (e1, e2) ->
    let _, c1 = infer e1 ctx in
    let t2, c2 = infer e2 ctx in
    let c = Constraint.unite c1 c2 in
    t2, c
  | Callcc (k, b) ->
    let kt = genvar () in
    let t, c = infer b (TypeContext.extend ctx k [] (TCont kt)) in
    t, c
  | Throw (k, x) ->
    let kt, c1 = infer k ctx in
    let xt, c2 = infer x ctx in
    let c = Constraint.unite c1 c2 in
    let rt = genvar () in
    rt, Constraint.add c kt (TCont xt)
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
