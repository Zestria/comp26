%{
[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

  open Ast
%}

%token <int> CONST
%token <string> ID
%token LE
%token GE
%token LT
%token GT
%token TIMES
%token DIVISION
%token PLUS
%token MINUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token FUN
%token REC
%token ARROW
%token EOF

%nonassoc IN
%left EQUALS LE LT GE GT
%left PLUS MINUS
%left TIMES DIVISION

%start <Ast.expr> prog
%start <Ast.program> program

%%

prog:
  | e = expr; EOF { e }
  ;

program:
  | rest = declarations { rest }
  | e = expr; rest = declarations { DExpr e :: rest }
  ;

declarations:
  | EOF { [] }
  | LET; x = ID; args = list(ID); EQUALS; body = expr; rest = declarations
      { DLet (x, List.fold_right (fun p e -> Abs (p, e)) args body) :: rest }
  | LET; REC; f = ID; p = ID; args = list(ID); EQUALS; body = expr; rest = declarations
      { DLetRec (f, p, List.fold_right (fun p e -> Abs (p, e)) args body) :: rest }
  ;


atom:
  | i = CONST { Const i }
  | x = ID { Var x }
  | LPAREN; e=expr; RPAREN {e}

app:
  | a = atom { a }
  | f = app; x = atom {App (f, x)}

unary:
  | a = app { a }
  | e = control { e }
  | MINUS; e = unary { Neg e }

operation:
  | u = unary {u}
  | e1 = operation; EQUALS; e2 = operation { BinOp (Eq, e1, e2) }
  | e1 = operation; LE; e2 = operation { BinOp (Le, e1, e2) }
  | e1 = operation; GE; e2 = operation { BinOp (Ge, e1, e2) }
  | e1 = operation; LT; e2 = operation { BinOp (Lt, e1, e2) }
  | e1 = operation; GT; e2 = operation { BinOp (Gt, e1, e2) }
  | e1 = operation; TIMES; e2 = operation { BinOp (Mul, e1, e2) }
  | e1 = operation; DIVISION; e2 = operation { BinOp (Div, e1, e2) }
  | e1 = operation; MINUS; e2 = operation {BinOp (Sub, e1, e2)}
  | e1 = operation; PLUS; e2 = operation { BinOp (Add, e1, e2) }

control:
  | LET; x = ID; args = list(ID); EQUALS; e1 = expr; IN; e2 = expr
      { Let (x, List.fold_right (fun p e -> Abs (p, e)) args e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | FUN; args = nonempty_list(ID); ARROW; body = expr {List.fold_right (fun x y -> Abs(x, y)) args body}
  | LET; REC; fun_id = ID; arg1 = ID; args = list(ID); EQUALS; body = expr; IN; e = expr
      { LetRec (fun_id, arg1, List.fold_right (fun x y -> Abs (x, y)) args body, e) }

expr:
  | op = operation {op} %prec IN
  ;
