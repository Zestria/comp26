[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Format

let list print formatter items =
  fprintf
    formatter
    "[%a]"
    (pp_print_list ~pp_sep:(fun formatter () -> pp_print_string formatter "; ") print)
    items
;;

let rec pattern formatter = function
  | PVar name -> fprintf formatter "PVar %S" (Name.to_string name)
  | PWildcard -> pp_print_string formatter "_"
  | PUnit -> pp_print_string formatter "()"
  | PTuple (a, b, rest) -> fprintf formatter "PTuple %a" (list pattern) (a :: b :: rest)
;;

let rec expression formatter = function
  | Constant (Int literal) ->
    fprintf formatter "Int %S" (Integer_literal.to_string literal)
  | Constant (Bool value) -> pp_print_bool formatter value
  | Constant Unit -> pp_print_string formatter "()"
  | Var name -> fprintf formatter "Var %S" (Name.to_string name)
  | Tuple (a, b, rest) -> fprintf formatter "Tuple %a" (list expression) (a :: b :: rest)
  | Apply (f, a, rest) ->
    fprintf formatter "Apply (%a, %a)" expression f (list expression) (a :: rest)
  | Fun { parameter; parameters; body } ->
    fprintf
      formatter
      "Fun (%a, %a)"
      (list pattern)
      (parameter :: parameters)
      expression
      body
  | Let (definition, body) ->
    fprintf formatter "Let (%a, %a)" binding definition expression body
  | If (condition, yes, no) ->
    let optional formatter = function
      | None -> pp_print_string formatter "None"
      | Some expression_ -> fprintf formatter "Some (%a)" expression expression_
    in
    fprintf formatter "If (%a, %a, %a)" expression condition expression yes optional no

and binding formatter { recursion; pattern = p; expression = e } =
  fprintf
    formatter
    "%s%a = %a"
    (match recursion with
     | Nonrecursive -> ""
     | Recursive -> "rec ")
    pattern
    p
    expression
    e
;;

let structure_item formatter = function
  | Definition definition -> fprintf formatter "Definition (%a)" binding definition
  | Evaluation value -> fprintf formatter "Evaluation (%a)" expression value
;;

let program = list structure_item
