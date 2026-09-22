(* Грамматика общего ядра подмножества OCaml (см. ADR 0001). Приоритеты и
   ассоциативность повторяют parser.mly компилятора OCaml, поэтому программа
   разбирается в то же дерево, что и у настоящего OCaml. *)

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
   [-(1)] тоже число. *)
let negate = function
  | Expr_constant (Const_int value) -> Expr_constant (Const_int (-value))
  | expression -> Expr_app (Expr_ident "~-", expression)
;;

let application function_ arguments =
  List.fold_left (fun applied argument -> Expr_app (applied, argument)) function_ arguments
;;
%}

%token <int> INT
(* Модуль [min_int], то есть [max_int + 1]: как положительное число он не
   помещается в [int] и допустим только сразу после унарного минуса. *)
%token INT_MIN_ABS
%token <string> LIDENT
%token <string> INFIXOP0 INFIXOP2 INFIXOP3 INFIXOP4
%token BARBAR AMPERAMPER PLUS MINUS EQUAL
%token LET REC AND IN FUN IF THEN ELSE TRUE FALSE
%token LPAREN RPAREN COMMA MINUSGREATER UNDERSCORE
%token EOF

(* От слабого к сильному. Тело [let], [fun] и ветка [else] тянутся вправо
   через запятую. *)
%nonassoc below_COMMA
%left COMMA
%right BARBAR
%right AMPERAMPER
%left INFIXOP0 EQUAL
%left INFIXOP2 PLUS MINUS
%left INFIXOP3
%right INFIXOP4
%nonassoc prec_unary

%start <Ast.structure> structure_eof
%start <Ast.expression> expression_eof

%%

structure_eof:
  | items = structure_item* EOF { items }

expression_eof:
  | e = expr EOF { e }

structure_item:
  | LET flag = rec_flag bindings = let_bindings
    { let first, rest = bindings in
      Structure_value (flag, first, rest) }

rec_flag:
  | { Non_recursive }
  | REC { Recursive }

let_bindings:
  | bindings = separated_nonempty_list(AND, let_binding)
    { List.hd bindings, List.tl bindings }

let_binding:
  | p = pattern EQUAL e = expr { { binding_pattern = p; binding_expression = e } }
  (* [let f x y = e] --- сахар для [let f = fun x y -> e]. *)
  | name = value_name first = simple_pattern rest = simple_pattern* EQUAL e = expr
    { { binding_pattern = Pat_var name; binding_expression = Expr_fun (first, rest, e) } }

(* Кортеж без скобок --- только на верхнем уровне выражения: его элементы не
   бывают кортежами сами, кроме хвоста [let], [fun] и [else]. *)
expr:
  | e = expr_no_tuple %prec below_COMMA { e }
  | items = expr_comma_list %prec below_COMMA
    { let first, second, rest = items in
      Expr_tuple (first, second, List.rev rest) }

(* Хвост кортежа копится в обратном порядке: левая рекурсия не растит стек
   парсера. *)
expr_comma_list:
  | items = expr_comma_list COMMA e = expr_no_tuple
    { let first, second, rest = items in
      first, second, e :: rest }
  | first = expr_no_tuple COMMA second = expr_no_tuple { first, second, [] }

(* [let], [fun] и [if] не бывают операндом оператора: [1 + if ...] вне ядра. *)
expr_no_tuple:
  | e = operand { e }
  | LET flag = rec_flag bindings = let_bindings IN body = expr
    { let first, rest = bindings in
      Expr_let (flag, first, rest, body) }
  | FUN first = simple_pattern rest = simple_pattern* MINUSGREATER body = expr
    { Expr_fun (first, rest, body) }
  | IF c = expr THEN t = expr ELSE e = expr { Expr_if (c, t, e) }

operand:
  | e = simple_expr { e }
  | e = application { e }
  | l = operand op = infix_operator r = operand { binary op l r }
  | MINUS e = operand %prec prec_unary { negate e }
  | MINUS INT_MIN_ABS { Expr_constant (Const_int min_int) }

(* [true], [false] и [()] для OCaml --- конструкторы, их применение к
   нескольким аргументам не разбирается. Поэтому без скобок они не бывают
   функцией: [true x] вне ядра, [(true) x] --- можно. *)
application:
  | f = function_expr arguments = simple_expr+ { application f arguments }

function_expr:
  | name = value_name { Expr_ident name }
  | value = INT { Expr_constant (Const_int value) }
  | LPAREN e = expr RPAREN { e }

simple_expr:
  | e = function_expr { e }
  | c = constructor { Expr_constant c }

pattern:
  | p = simple_pattern { p }
  | items = pattern_comma_list
    { let first, second, rest = items in
      Pat_tuple (first, second, List.rev rest) }

pattern_comma_list:
  | items = pattern_comma_list COMMA p = simple_pattern
    { let first, second, rest = items in
      first, second, p :: rest }
  | first = simple_pattern COMMA second = simple_pattern { first, second, [] }

(* Отрицательная константа в паттерне --- только в скобках: [(-1)]. *)
simple_pattern:
  | name = value_name { Pat_var name }
  | UNDERSCORE { Pat_wildcard }
  | c = constant { Pat_constant c }
  | LPAREN MINUS value = INT RPAREN { Pat_constant (Const_int (-value)) }
  | LPAREN MINUS INT_MIN_ABS RPAREN { Pat_constant (Const_int min_int) }
  | LPAREN p = pattern RPAREN { p }

constant:
  | value = INT { Const_int value }
  | c = constructor { c }

constructor:
  | TRUE { Const_bool true }
  | FALSE { Const_bool false }
  | LPAREN RPAREN { Const_unit }

value_name:
  | name = LIDENT { name }
  | LPAREN op = infix_operator RPAREN { op }

%inline infix_operator:
  | op = INFIXOP0 { op }
  | op = INFIXOP2 { op }
  | op = INFIXOP3 { op }
  | op = INFIXOP4 { op }
  | BARBAR { "||" }
  | AMPERAMPER { "&&" }
  | PLUS { "+" }
  | MINUS { "-" }
  | EQUAL { "=" }
