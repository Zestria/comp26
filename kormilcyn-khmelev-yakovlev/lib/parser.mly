
%{
  open Ast
%}

(* TODO: functions *)
%token <int> CONST
%token <string> ID
%token LE
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token FUN
%token ARROW
%token EOF

%nonassoc IN
%nonassoc ELSE
%left EQUALS LE
%left PLUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;


expr:
  | i = CONST { Const i }
  | x = ID { Var x }
  | e1 = expr; EQUALS; e2 = expr { BinOp (Eq, e1, e2) }
  | e1 = expr; LE; e2 = expr { BinOp (Le, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { BinOp (Mul, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { BinOp (Add, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | FUN; args = nonempty_list(ID); ARROW; body = expr {List.fold_right (fun x y -> Abs(x, y)) args body}
  | LPAREN; e=expr; RPAREN {e}
  ;
