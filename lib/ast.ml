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
  (* Lambda calculus *)
  | Id of string
  | Lam of string * expr
  | App of expr * expr
  (* Standard *)
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Binop of binop * expr * expr
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
