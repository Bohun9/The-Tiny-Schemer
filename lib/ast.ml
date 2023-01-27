type binop =
  | Add
  | Sub
  | Mult
  | Leq
  | Geq

type expr =
  (* Atomic *)
  | Unit
  | Int of int
  | Bool of bool
  | Error
  (* Lambda calculus *)
  | Id of string
  | Lam of string * expr
  | App of expr * expr
  (* Standard *)
  | Pair of expr * expr
  | First of expr
  | Second of expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Binop of binop * expr * expr
  (* Lists *)
  | Nil
  | Empty of expr
  | Car of expr
  | Cdr of expr
  | Cons of expr * expr
  (* Recursion *)
  | Letrec of string * string * expr * expr
  (* Mutable records *)
  | Ref of expr
  | Deref of expr
  | Set of expr * expr
  | Begin of expr * expr
  (* Continuations *)
  | Callcc of string * expr
  | Throw of expr * expr
  | IsCont of expr
