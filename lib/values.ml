open Ast

exception IdentNotInScope

module Environment : sig
  type 'a t

  val empty : 'a t
  val extend : 'a t -> string -> 'a -> 'a t
  val query : 'a t -> string -> 'a
end = struct
  type 'a t = (string * 'a) list

  let empty = []
  let extend (env : 'a t) (x : string) (v : 'a) : 'a t = (x, v) :: env

  let query (env : 'a t) (x : string) =
    match List.assoc_opt x env with
    | Some v -> v
    | None -> raise IdentNotInScope
  ;;
end

exception LocalizationNotInStore

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
  | VLoc of int
  | VClo of string * expr * value Environment.t
  | VCont of (result -> result)
  | VCloRec of string * string * expr * value Environment.t

and result = value * value Store.t
