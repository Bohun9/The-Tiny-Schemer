%{
open Ast
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token GEQ
%token TIMES  
%token PLUS
%token MINUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token DOUBLEEQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF
%token UNIT
%token SET
%token SEMICOLON
%token BANG
%token REF
%token FUN
%token LETREC
%token CALCC
%token THROW
%token COMMA
%token ANGLELEFT
%token ANGLERIGHT
%token FIRST
%token SECOND
%token NIL
%token EMPTY
%token CONS
%token CAR
%token CDR
%token ERROR
%token ISCONT
%token OR
%token AND
%token XOR
%token NOT

%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left MINUS
%left TIMES  

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
    | UNIT { Unit }
    | i = INT { Int i }
    | x = ID { Id x }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | NIL { Nil }
    | EMPTY; e = expr { Empty e }
    | CAR; e = expr { Car e }
    | CDR; e = expr { Cdr e }
    | CONS; e1 = expr; e2 = expr { Cons (e1, e2) }
    | FUN; x = ID; e = expr { Lam (x, e) }
    | NOT; e = expr { Unop (Not, e) }
    | e1 = expr; e2 = expr { App (e1, e2) }
    | e1 = expr; OR; e2 = expr { Binop (Or, e1, e2) }
    | e1 = expr; AND; e2 = expr { Binop (And, e1, e2) }
    | e1 = expr; XOR; e2 = expr { Binop (Xor, e1, e2) }
    | e1 = expr; GEQ; e2 = expr { Binop (Geq, e1, e2) }
    | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
    | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) } 
    | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
    | e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) }
    | e1 = expr; DOUBLEEQUALS; e2 = expr { Binop (Eq, e1, e2) }
    | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
    | LETREC; f = ID; x = ID; e1 = expr; IN; e2 = expr { Letrec (f, x, e1, e2) }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
    | REF; e = expr { (Ref e) }
    | BANG; e = expr { (Deref e) }
    | e1 = expr; SET; e2 = expr { Set (e1, e2) }
    | e1 = expr; SEMICOLON; e2 = expr { Begin (e1, e2) }
    | CALCC; k = ID; e = expr { Callcc (k, e) }
    | THROW; e1 = expr; e2 = expr { Throw (e1, e2) }
    | LPAREN; e=expr; RPAREN {e} 
    | ANGLELEFT; e1 = expr; COMMA; e2 = expr; ANGLERIGHT { Pair (e1, e2) }
    | FIRST; e = expr { First e }
    | SECOND; e = expr { Second e }
    | ERROR; { Error }
    | ISCONT; e = expr { IsCont (e) }
    ;
