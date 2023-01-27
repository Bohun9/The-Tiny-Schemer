type ttype =
  | TInt
  | TBool
  | TUnit
  | TError
  | TVar of string
  | TList of ttype
  | TPair of ttype * ttype
  | TFun of ttype * ttype
  | TRef of ttype
  | TCont of ttype

type tscheme = Forall of string list * ttype

let rec free_type_variables (t : ttype) : string list =
  match t with
  | TInt -> []
  | TBool -> []
  | TUnit -> []
  | TError -> []
  | TVar x -> [ x ]
  | TList t -> free_type_variables t
  | TPair (t1, t2) -> Utils.unite_lists (free_type_variables t1) (free_type_variables t2)
  | TFun (t1, t2) -> Utils.unite_lists (free_type_variables t1) (free_type_variables t2)
  | TRef t -> free_type_variables t
  | TCont t -> free_type_variables t
;;

module Substitution : sig
  type t

  val empty : t
  val apply : t -> ttype -> ttype
  val ( $$ ) : t -> t -> t
  val ( >> ) : string -> ttype -> t
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
      | TError -> TError
      | TVar x -> query subst x
      | TList t -> TList (applier t)
      | TPair (t1, t2) -> TPair (applier t1, applier t2)
      | TFun (t1, t2) -> TFun (applier t1, applier t2)
      | TRef t -> TRef (applier t)
      | TCont t -> TCont (applier t)
    in
    applier t
  ;;

  let ( $$ ) s1 s2 = List.fold_left (fun r (x, t) -> (x, apply s1 t) :: r) [] s2 @ s1
  let ( >> ) x t = [ x, t ]
  let to_list (subst : t) = subst
end

let ( $$ ) = Substitution.( $$ )
let ( >> ) = Substitution.( >> )
