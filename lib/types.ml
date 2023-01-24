type ttype =
  | TInt
  | TBool
  | TUnit
  | TVar of string
  | TFun of ttype * ttype
  | TRef of ttype
  | TCont of ttype

type ptype = Forall of string list * ttype

let rec free_type_variables (t : ttype) : string list =
  match t with
  | TInt -> []
  | TBool -> []
  | TUnit -> []
  | TVar x -> [ x ]
  | TFun (t1, t2) -> Utils.unite_lists (free_type_variables t1) (free_type_variables t2)
  | TRef t -> free_type_variables t
  | TCont t -> free_type_variables t
;;

module Substitution : sig
  type t

  val empty : t
  val apply : t -> ttype -> ttype
  val compose : t -> string -> ttype -> t
  val to_list : t -> (string * ttype) list
end = struct
  type t = (string * ttype) list

  let empty = []

  let rec query (subst : t) (x : string) =
    match subst with
    | [] -> TVar x
    | (y, t) :: rest -> if x = y then t else query rest x
  ;;

  let apply (subst : t) (t : ttype) =
    let rec applier (t : ttype) =
      match t with
      | TInt -> TInt
      | TBool -> TBool
      | TUnit -> TUnit
      | TVar x -> query subst x
      | TFun (t1, t2) -> TFun (applier t1, applier t2)
      | TRef t -> TRef (applier t)
      | TCont t -> TCont (applier t)
    in
    applier t
  ;;

  let compose (subst : t) (x : string) (t : ttype) = (x, apply subst t) :: subst
  let to_list (subst : t) = subst
end
