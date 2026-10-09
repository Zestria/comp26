%{
[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let apply_binop op left right = EApply (EApply (EIdent op, left), right)

let curry patterns body =
  List.fold_right (fun pattern acc -> EFun (pattern, acc)) patterns body
;;
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token LET REC AND IN
%token IF THEN ELSE
%token MATCH WITH FUNCTION FUN
%token NOT
%token UNDERSCORE
%token LPAREN RPAREN COMMA BAR ARROW SEMISEMI
%token OROR ANDAND
%token EQ NEQ LE GE LT GT
%token PLUS MINUS STAR SLASH
%token EOF

(* Open constructs ([match], [function], [if]) extend as far to the right as
   possible, like the top-down parser: a nested [match] takes all following
   cases, and [else] belongs to the nearest [if]. *)
%nonassoc below_BAR
%nonassoc BAR
%nonassoc THEN
%nonassoc ELSE

(* Default operator table, see [Syntax.default_table]. *)
%right OROR
%right ANDAND
%left EQ NEQ LE GE LT GT
%left PLUS MINUS
%left STAR SLASH

%start <Ast.structure> structure

%%

(* ==================== structure ==================== *)

(* Items are separated by any number of [;;]. Without [;;] the next item has
   to start with a keyword, otherwise it would continue the previous one. *)
structure:
  | EOF { [] }
  | item = structure_item; rest = after_item { item :: rest }

after_item:
  | EOF { [] }
  | SEMISEMI; rest = after_semi { rest }
  | item = keyword_item; rest = after_item { item :: rest }

after_semi:
  | EOF { [] }
  | SEMISEMI; rest = after_semi { rest }
  | item = structure_item; rest = after_item { item :: rest }

structure_item:
  | item = keyword_item { item }
  | e = infix_expr { SEval e }

keyword_item:
  | LET; flag = rec_flag; first = value_binding; rest = and_bindings
    { SValue (flag, first, rest) }
  | e = keyword_expr { SEval e }

(* ==================== expressions ==================== *)

expr:
  | e = keyword_expr { e }
  | e = infix_expr { e }

keyword_expr:
  | FUN; parameter = pattern; parameters = list(pattern); ARROW; body = expr
    { EFun (parameter, curry parameters body) }
  | LET; flag = rec_flag; first = value_binding; rest = and_bindings; IN; body = expr
    { ELet (flag, first, rest, body) }
  | FUNCTION; cases = cases
    { let first, rest = cases in EFunction (first, rest) }
  | MATCH; subject = expr; WITH; cases = cases
    { let first, rest = cases in EMatch (subject, first, rest) }
  | IF; condition = expr; THEN; then_branch = expr
    { EIf (condition, then_branch, None) }
  | IF; condition = expr; THEN; then_branch = expr; ELSE; else_branch = expr
    { EIf (condition, then_branch, Some else_branch) }

(* Operands of infix operators can't be open constructs: [1 + if ...] is
   rejected by the top-down parser as well. *)
infix_expr:
  | left = infix_expr; op = binop; right = infix_expr { apply_binop op left right }
  | e = prefix_expr { e }

%inline binop:
  | OROR { "||" }
  | ANDAND { "&&" }
  | EQ { "=" }
  | NEQ { "<>" }
  | LE { "<=" }
  | GE { ">=" }
  | LT { "<" }
  | GT { ">" }
  | PLUS { "+" }
  | MINUS { "-" }
  | STAR { "*" }
  | SLASH { "/" }

(* Prefix ops bind tighter than infix but looser than application:
   [-a * b] is [(-a) * b], while [-f x] is [-(f x)]. *)
prefix_expr:
  | op = prefix_op; operand = application { EApply (EIdent op, operand) }
  | e = application { e }

prefix_op:
  | PLUS { "+" }
  | MINUS { "-" }
  | NOT { "not" }

application:
  | e = atom { e }
  | fn = application; argument = atom { EApply (fn, argument) }

atom:
  | c = constant { EConst c }
  | name = IDENT { EIdent name }
  | LPAREN; op = operator; RPAREN { EIdent op }
  | LPAREN; e = expr; RPAREN { e }
  | LPAREN; first = expr; COMMA; second = expr; rest = list(preceded(COMMA, expr)); RPAREN
    { ETuple (first, second, rest) }

operator:
  | op = binop { op }
  | NOT { "not" }

constant:
  | n = INT { CInt n }
  | TRUE { CBool true }
  | FALSE { CBool false }
  | LPAREN; RPAREN { CUnit }

(* ==================== bindings and cases ==================== *)

rec_flag:
  | REC { Rec }
  | { NonRec }

value_binding:
  | pat = pattern; parameters = list(pattern); EQ; body = expr
    { { vb_pat = pat; vb_expr = curry parameters body } }

and_bindings:
  | rest = list(preceded(AND, value_binding)) { rest }

cases:
  | first = case; rest = reversed_cases %prec below_BAR { first, List.rev rest }

reversed_cases:
  | { [] }
  | rest = reversed_cases; c = case { c :: rest }

case:
  | BAR; case_pat = pattern; ARROW; case_expr = expr { { case_pat; case_expr } }

(* ==================== pattern ==================== *)

pattern:
  | UNDERSCORE { PAny }
  | name = IDENT { PVar name }
  | c = constant { PConstant c }
  | LPAREN; p = pattern; RPAREN { p }
  | LPAREN; first = pattern; COMMA; second = pattern;
    rest = list(preceded(COMMA, pattern)); RPAREN
    { PTuple (first, second, rest) }
