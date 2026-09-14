%{
  (** Copyright 2023-2025, Kakadu and contributors *)
  (** SPDX-License-Identifier: LGPL-3.0-or-later *)

  open Common

  let curry params body = List.fold_right elambda params body
%}

%token <string> IDENT
%token <int> INT
%token IF THEN ELSE
%token LET REC IN
%token FUN RIGHTARROW
%token UNIT
%token PLUS MINUS TIMES DIV
%token EQ LT GT
%token LPAREN RPAREN
%token EOF

%nonassoc IN RIGHTARROW
%nonassoc THEN
%nonassoc ELSE
%left EQ LT GT
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%type <Common.expr> expr app_expr atom

%start <Common.expr> program

%%

program:
  | e = expr; EOF { e }
  ;

expr:
  | LET; REC; f = IDENT; ps = IDENT*; EQ; e1 = expr; IN; e2 = expr
      { eletrec f (curry ps e1) e2 }
  | LET; x = IDENT; ps = IDENT*; EQ; e1 = expr; IN; e2 = expr
      { elet x (curry ps e1) e2 }
  | IF; c = expr; THEN; t = expr
      { eif c t eunit }
  | IF; c = expr; THEN; t = expr; ELSE; f = expr
      { eif c t f }
  | FUN; ps = IDENT+; RIGHTARROW; body = expr
      { curry ps body }
  | a = expr; EQ; b = expr    { eeq a b }
  | a = expr; LT; b = expr    { elt a b }
  | a = expr; GT; b = expr    { egt a b }
  | a = expr; PLUS; b = expr  { eadd a b }
  | a = expr; MINUS; b = expr { esub a b }
  | a = expr; TIMES; b = expr { emul a b }
  | a = expr; DIV; b = expr   { ediv a b }
  | MINUS; e = expr %prec UMINUS { euminus e }
  | e = app_expr { e }
  ;

app_expr:
  | f = app_expr; a = atom { eapp f a }
  | e = atom { e }
  ;

atom:
  | x = IDENT { Ident x }
  | n = INT   { econsti n }
  | UNIT      { eunit }
  | LPAREN; e = expr; RPAREN { e }
  ;
