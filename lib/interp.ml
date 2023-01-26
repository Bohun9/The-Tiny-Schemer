open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let run_dynamic (e : string) = e |> parse |> Eval.evaluate

let run_static (e : string) =
  let _ = e |> parse |> Typecheck.typeof in
  e |> run_dynamic
;;
