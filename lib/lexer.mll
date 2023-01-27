{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | "()" { UNIT }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | ":=" { SET }
  | ";" { SEMICOLON }
  | "==" { DOUBLEEQUALS }
  | "=" { EQUALS }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { MINUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "!" { BANG }
  | "<" { ANGLELEFT }
  | ">" { ANGLERIGHT }
  | "," { COMMA }
  | "empty?" { EMPTY }
  | "cont?" { ISCONT }
  | "[]" { NIL }
  | "cons" { CONS }
  | "car" { CAR }
  | "cdr" { CDR }
  | "fst" { FIRST }
  | "snd" { SECOND }
  | "ref" { REF }
  | "fun" { FUN }
  | "let rec" { LETREC }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "call/cc" { CALCC }
  | "throw" { THROW }
  | "error" { ERROR }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
