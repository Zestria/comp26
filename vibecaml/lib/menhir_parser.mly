(* Грамматика подмножества OCaml. Приоритеты и ассоциативность повторяют
   parser.mly компилятора OCaml, поэтому программа разбирается в то же дерево,
   что и у настоящего OCaml. *)

%{
[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Шапка с лицензией здесь, а не в начале файла: menhir копирует в
   сгенерированный .ml только этот блок. *)

open Ast

let binary operator left right = Expr_app (Expr_app (Expr_ident operator, left), right)

(* Минус перед целой константой сворачивается в константу, как в OCaml:
   [-1] --- это число, [-x] --- применение [~-]. Скобки прозрачны, поэтому
   [-(1)] тоже число. Исключение --- [min_int]: его отрицание не
   представимо, поэтому оставляем честное применение [~-]. OCaml такой литерал
   отвергает, но дерево корректно и печатается обратно. *)
let negate = function
  | Expr_constant (Const_int value) when value <> min_int ->
    Expr_constant (Const_int (-value))
  | expression -> Expr_app (Expr_ident "~-", expression)
;;

let plus = function
  | Expr_constant (Const_int _) as constant -> constant
  | expression -> Expr_app (Expr_ident "~+", expression)
;;

let application function_ arguments =
  List.fold_left (fun applied argument -> Expr_app (applied, argument)) function_ arguments
;;
%}

%token <int> INT
(* Модуль [min_int], то есть [max_int + 1]: как положительное число он не
   помещается в [int] и допустим только после унарного минуса. *)
%token INT_MIN_ABS
%token <string> LIDENT
%token <string> INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4
%token <string> PREFIXOP HASHOP
(* [||] и [or], [&&] и [&] --- синонимы с общим приоритетом. *)
%token <string> BARBAR AMPERAMPER
%token PLUS PLUSDOT MINUS MINUSDOT EQUAL COLONEQUAL BANG
%token LET REC AND IN FUN IF THEN ELSE TRUE FALSE BEGIN END
%token LPAREN RPAREN COMMA MINUSGREATER UNDERSCORE SEMISEMI
%token EOF

(* От слабого к сильному. Тело [let] и [fun] тянется вправо, насколько
   возможно; [else]-ветка --- до запятой. *)
%nonassoc IN
%nonassoc below_FUN
%nonassoc ELSE
%right COLONEQUAL
%nonassoc below_COMMA
%left COMMA
%right BARBAR
%right AMPERAMPER
%left INFIXOP0 EQUAL
%right INFIXOP1
%left INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT
%left INFIXOP3
%right INFIXOP4
%nonassoc prec_unary
%left HASHOP
(* Префиксный оператор сильнее [##]: [!x ## y] --- это [(!x) ## y]. *)
%nonassoc BANG PREFIXOP

%start <Ast.structure> structure_eof
%start <Ast.expression> expression_eof

%%

structure_eof:
  | SEMISEMI* items = terminated(structure_item, SEMISEMI*)* EOF { items }

expression_eof:
  | e = expr EOF { e }

structure_item:
  | LET flag = rec_flag bindings = separated_nonempty_list(AND, let_binding)
    { Structure_value (flag, List.hd bindings, List.tl bindings) }

rec_flag:
  | { Non_recursive }
  | REC { Recursive }

let_binding:
  | p = pattern EQUAL e = expr { { binding_pattern = p; binding_expression = e } }
  (* [let f x y = e] --- сахар для [let f = fun x y -> e]. *)
  | name = value_name first = simple_pattern rest = simple_pattern* EQUAL e = expr
    { { binding_pattern = Pat_var name; binding_expression = Expr_fun (first, rest, e) } }

expr:
  | e = simple_expr { e }
  | e = application { e }
  | LET flag = rec_flag bindings = separated_nonempty_list(AND, let_binding) IN body = expr
    { Expr_let (flag, List.hd bindings, List.tl bindings, body) }
  | FUN first = simple_pattern rest = simple_pattern* MINUSGREATER body = expr
    %prec below_FUN
    { Expr_fun (first, rest, body) }
  | IF c = expr THEN t = expr ELSE e = expr { Expr_if (c, t, e) }
  | items = expr_comma_list %prec below_COMMA
    { let first, second, rest = items in
      Expr_tuple (first, second, List.rev rest) }
  | l = expr op = infix_operator r = expr { binary op l r }
  | MINUS e = expr %prec prec_unary { negate e }
  | MINUS INT_MIN_ABS { Expr_constant (Const_int min_int) }
  | PLUS e = expr %prec prec_unary { plus e }
  | MINUSDOT e = expr %prec prec_unary { Expr_app (Expr_ident "~-.", e) }
  | PLUSDOT e = expr %prec prec_unary { Expr_app (Expr_ident "~+.", e) }

(* Хвост кортежа копится в обратном порядке: левая рекурсия не растит стек
   парсера. *)
expr_comma_list:
  | items = expr_comma_list COMMA e = expr
    { let first, second, rest = items in
      first, second, e :: rest }
  | first = expr COMMA second = expr { first, second, [] }

application:
  | f = simple_expr arguments = simple_expr+ { application f arguments }

simple_expr:
  | name = value_name { Expr_ident name }
  | c = constant { Expr_constant c }
  | LPAREN e = expr RPAREN { e }
  | BEGIN e = expr END { e }
  | BEGIN END { Expr_constant Const_unit }
  | op = PREFIXOP e = simple_expr { Expr_app (Expr_ident op, e) }
  | BANG e = simple_expr { Expr_app (Expr_ident "!", e) }
  | l = simple_expr op = HASHOP r = simple_expr { binary op l r }

pattern:
  | p = simple_pattern { p }
  | items = pattern_comma_list %prec below_COMMA
    { let first, second, rest = items in
      Pat_tuple (first, second, List.rev rest) }

pattern_comma_list:
  | items = pattern_comma_list COMMA p = pattern
    { let first, second, rest = items in
      first, second, p :: rest }
  | first = pattern COMMA second = pattern { first, second, [] }

simple_pattern:
  | name = value_name { Pat_var name }
  | UNDERSCORE { Pat_wildcard }
  | c = signed_constant { Pat_constant c }
  | LPAREN p = pattern RPAREN { p }

constant:
  | value = INT { Const_int value }
  | TRUE { Const_bool true }
  | FALSE { Const_bool false }
  | LPAREN RPAREN { Const_unit }

signed_constant:
  | c = constant { c }
  | MINUS value = INT { Const_int (-value) }
  | MINUS INT_MIN_ABS { Const_int min_int }
  | PLUS value = INT { Const_int value }

value_name:
  | name = LIDENT { name }
  | LPAREN op = operator RPAREN { op }

operator:
  | op = PREFIXOP { op }
  | BANG { "!" }
  | op = infix_operator { op }
  | op = HASHOP { op }

%inline infix_operator:
  | op = INFIXOP0 { op }
  | op = INFIXOP1 { op }
  | op = INFIXOP2 { op }
  | op = INFIXOP3 { op }
  | op = INFIXOP4 { op }
  | op = BARBAR { op }
  | op = AMPERAMPER { op }
  | PLUS { "+" }
  | PLUSDOT { "+." }
  | MINUS { "-" }
  | MINUSDOT { "-." }
  | EQUAL { "=" }
  | COLONEQUAL { ":=" }
