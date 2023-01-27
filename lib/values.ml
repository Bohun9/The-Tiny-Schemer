open Ast

exception IdentNotInScope of string
exception LocalizationNotInStore

module Environment : sig
  type 'a t

  val empty : 'a t
  val ( || ) : 'a t -> string * 'a -> 'a t
  val query : 'a t -> string -> 'a
end = struct
  type 'a t = (string * 'a) list

  let empty = []
  let ( || ) (env : 'a t) (b : string * 'a) : 'a t = b :: env

  let query (env : 'a t) (x : string) =
    match List.assoc_opt x env with
    | Some v -> v
    | None -> raise (IdentNotInScope x)
  ;;
end

let ( || ) = Environment.( || )

module Store : sig
  type 'a t

  val empty : 'a t
  val add : 'a t -> 'a -> 'a t * int
  val query : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
end = struct
  type 'a t = (int * 'a) list

  let empty = []

  let add (st : 'a t) (v : 'a) =
    let loc = List.length st in
    (loc, v) :: st, loc
  ;;

  let query (st : 'a t) (loc : int) =
    match List.assoc_opt loc st with
    | Some v -> v
    | None -> raise LocalizationNotInStore
  ;;

  let set (st : 'a t) (loc : int) (v : 'a) = (loc, v) :: st
end

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VError
  | VPair of value * value
  | VNil
  | VCons of value * value
  | VLoc of int
  | VClo of string * expr * value Environment.t
  | VCont of (result -> result)
  | VCloRec of string * string * expr * value Environment.t

and result = value * value Store.t

let rec to_string (v : value) : string =
  match v with
  | VInt n -> "Int " ^ string_of_int n
  | VBool b -> "Bool " ^ Bool.to_string b
  | VUnit -> "Unit"
  | VError -> "Error"
  | VPair (v1, v2) -> "<" ^ to_string v1 ^ "," ^ to_string v2 ^ ">"
  | VNil -> "Nil"
  | VCons (v1, v2) -> "Cons(" ^ to_string v1 ^ "," ^ to_string v2 ^ ")"
  | VLoc loc -> "Loc " ^ string_of_int loc
  | VClo _ -> "Clo"
  | VCont _ -> "Cont"
  | VCloRec _ -> "CloRec"
;;
