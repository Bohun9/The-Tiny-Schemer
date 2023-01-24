open Types

module Constraint : sig
  type t

  val empty : t list
  val add : t list -> ttype -> ttype -> t list
  val unite : t list -> t list -> t list
  val to_pair : t -> ttype * ttype
end = struct
  type t = ttype * ttype

  let empty = []
  let add cs t1 t2 = (t1, t2) :: cs
  let unite c1 c2 = c1 @ c2
  let to_pair c = c
end

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
        Substitution.compose (unify cs) x t
      | t, TVar x when List.mem x (free_type_variables t) = false ->
        Substitution.compose (unify cs) x t
      | TFun (t11, t12), TFun (t21, t22) ->
        unify (Constraint.add (Constraint.add cs t11 t21) t12 t22)
      | TRef t1, TRef t2 -> unify (Constraint.add cs t1 t2)
      | TCont t1, TCont t2 -> unify (Constraint.add cs t1 t2)
      | _ -> raise Unification_failed)
;;
