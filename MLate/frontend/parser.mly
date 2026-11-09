%{
[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let desugar_fun args body =
  List.fold_right (fun arg body -> Fun (arg, body)) args body
%}

%token LET REC IN FUN IF THEN ELSE
%token EQUAL ARROW PLUS MINUS TIMES LT LPAREN RPAREN EOF
%token <int> INT
%token <string> IDENT

%start <Ast.program> program

%%

program:
  | declarations = list(binding); EOF { declarations }

binding:
  | LET; flag = rec_flag; name = IDENT; args = list(IDENT); EQUAL; value = expr
      { { flag; name; value = desugar_fun args value } }

rec_flag:
  | REC { Rec }
  |     { Nonrec }

expr:
  | declaration = binding; IN; body = expr { Let (declaration, body) }
  | IF; condition = expr; THEN; then_branch = expr; ELSE; else_branch = expr
      { If (condition, then_branch, else_branch) }
  | FUN; args = nonempty_list(IDENT); ARROW; body = expr
      { desugar_fun args body }
  | value = comparison { value }

comparison:
  | value = additive { value }
  | left = additive; LT; right = additive { Binop (Lt, left, right) }

additive:
  | value = multiplicative { value }
  | left = additive; PLUS; right = multiplicative { Binop (Plus, left, right) }
  | left = additive; MINUS; right = multiplicative { Binop (Minus, left, right) }

multiplicative:
  | value = application { value }
  | left = multiplicative; TIMES; right = application { Binop (Times, left, right) }

application:
  | value = atom { value }
  | fn = application; arg = atom { App (fn, arg) }

atom:
  | value = INT { Int value }
  | name = IDENT { Var name }
  | LPAREN; value = expr; RPAREN { value }
