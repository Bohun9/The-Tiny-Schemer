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

  let query (subst : t) (x : string) : ttype =
    match List.assoc_opt x subst with
    | Some y -> y
    | None -> TVar x
  ;;

  let apply (subst : t) (t : ttype) : ttype =
    let rec app (t : ttype) : ttype =
      match t with
      | TInt -> TInt
      | TBool -> TBool
      | TUnit -> TUnit
      | TError -> TError
      | TVar x -> query subst x
      | TList t -> TList (app t)
      | TPair (t1, t2) -> TPair (app t1, app t2)
      | TFun (t1, t2) -> TFun (app t1, app t2)
      | TRef t -> TRef (app t)
      | TCont t -> TCont (app t)
    in
    app t
  ;;

  let ( $$ ) s1 s2 = List.fold_left (fun r (x, t) -> (x, apply s1 t) :: r) [] s2 @ s1
  let ( >> ) x t = [ x, t ]
  let to_list (subst : t) = subst
end

let ( $$ ) = Substitution.( $$ )
let ( >> ) = Substitution.( >> )
