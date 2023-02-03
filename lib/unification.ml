open Types

module Constraint : sig
  type t

  val empty : t list
  val ( == ) : ttype -> ttype -> t list
  val ( ++ ) : t list -> t list -> t list
  val apply : t list -> Substitution.t -> t list
  val to_pair : t -> ttype * ttype
  val to_list : t list -> (ttype * ttype) list
end = struct
  type t = ttype * ttype

  let empty = []
  let ( == ) (t1 : ttype) (t2 : ttype) = [ t1, t2 ]
  let ( ++ ) (c1 : t list) (c2 : t list) = c2 @ c1

  let apply (cs : t list) (subst : Substitution.t) : t list =
    List.fold_left
      (fun cs (t1, t2) ->
        (Substitution.apply subst t1, Substitution.apply subst t2) :: cs)
      []
      cs
  ;;

  let to_pair c = c
  let to_list c = c
end

let ( == ) = Constraint.( == )
let ( ++ ) = Constraint.( ++ )

exception Unification_failed

let rec unify (cs : Constraint.t list) : Substitution.t =
  match cs with
  | [] -> Substitution.empty
  | eq :: cs ->
    let t1, t2 = Constraint.to_pair eq in
    if t1 = t2
    then unify cs
    else (
      match t1, t2 with
      | TVar x, t when List.mem x (free_type_variables t) = false ->
        unify (Constraint.apply cs (x >> t)) $$ (x >> t)
      | t, TVar x when List.mem x (free_type_variables t) = false ->
        unify (Constraint.apply cs (x >> t)) $$ (x >> t)
      | TFun (t11, t12), TFun (t21, t22) -> unify (cs ++ (t11 == t21) ++ (t12 == t22))
      | TPair (t11, t12), TPair (t21, t22) -> unify (cs ++ (t11 == t21) ++ (t12 == t22))
      | TRef t1, TRef t2 -> unify (cs ++ (t1 == t2))
      | TCont t1, TCont t2 -> unify (cs ++ (t1 == t2))
      | TList t1, TList t2 -> unify (cs ++ (t1 == t2))
      | _ -> raise Unification_failed)
;;
