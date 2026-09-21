[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(* Инфиксные операторы, записанные буквами. Голыми их не напечатать, как и
   символьные: [mod] вне скобок --- ключевое слово, а не идентификатор. *)
let keyword_operators = [ "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"; "or" ]

let is_operator name =
  List.mem name keyword_operators
  || (String.length name > 0 && String.contains "!#$%&*+-./:<=>?@^|~" name.[0])
;;

(* Пробелы вокруг оператора обязательны: скобка, слитая со звёздочкой, открывает
   комментарий. *)
let pp_identifier ppf name =
  if is_operator name
  then Format.fprintf ppf "( %s )" name
  else Format.pp_print_string ppf name
;;

(* Отрицательное число берём в скобки: иначе [f (-1)] напечатается как
   [f -1], а это вычитание. *)
let pp_constant ppf = function
  | Const_int value when value < 0 -> Format.fprintf ppf "(%d)" value
  | Const_int value -> Format.fprintf ppf "%d" value
  | Const_bool value -> Format.fprintf ppf "%b" value
  | Const_unit -> Format.pp_print_string ppf "()"
;;

let pp_tail separator printer ppf values =
  List.iter (fun value -> Format.fprintf ppf "%s%a" separator printer value) values
;;

let pp_rec_flag ppf = function
  | Non_recursive -> ()
  | Recursive -> Format.pp_print_string ppf " rec"
;;

let rec pp_pattern ppf = function
  | Pat_wildcard -> Format.pp_print_string ppf "_"
  | Pat_var name -> pp_identifier ppf name
  | Pat_constant constant -> pp_constant ppf constant
  | Pat_tuple (first, second, rest) ->
    Format.fprintf
      ppf
      "(%a, %a%a)"
      pp_pattern
      first
      pp_pattern
      second
      (pp_tail ", " pp_pattern)
      rest
;;

let rec pp_expression ppf = function
  | Expr_constant constant -> pp_constant ppf constant
  | Expr_ident name -> pp_identifier ppf name
  | Expr_let (rec_flag, first, rest, body) ->
    Format.fprintf
      ppf
      "(let%a %a in %a)"
      pp_rec_flag
      rec_flag
      pp_value_bindings
      (first, rest)
      pp_expression
      body
  | Expr_fun (first, rest, body) ->
    Format.fprintf
      ppf
      "(fun %a%a -> %a)"
      pp_pattern
      first
      (pp_tail " " pp_pattern)
      rest
      pp_expression
      body
  | Expr_app (function_, argument) ->
    Format.fprintf ppf "(%a %a)" pp_expression function_ pp_expression argument
  | Expr_if (condition, then_branch, else_branch) ->
    Format.fprintf
      ppf
      "(if %a then %a else %a)"
      pp_expression
      condition
      pp_expression
      then_branch
      pp_expression
      else_branch
  | Expr_tuple (first, second, rest) ->
    Format.fprintf
      ppf
      "(%a, %a%a)"
      pp_expression
      first
      pp_expression
      second
      (pp_tail ", " pp_expression)
      rest

and pp_value_binding ppf { binding_pattern; binding_expression } =
  Format.fprintf ppf "%a = %a" pp_pattern binding_pattern pp_expression binding_expression

and pp_value_bindings ppf (first, rest) =
  Format.fprintf ppf "%a%a" pp_value_binding first (pp_tail " and " pp_value_binding) rest
;;

let pp_structure_item ppf = function
  | Structure_value (rec_flag, first, rest) ->
    Format.fprintf ppf "let%a %a" pp_rec_flag rec_flag pp_value_bindings (first, rest)
;;

let pp_structure ppf items =
  List.iteri
    (fun index item ->
       if index > 0 then Format.fprintf ppf "\n";
       pp_structure_item ppf item)
    items
;;

let string_of_expression expression = Format.asprintf "%a" pp_expression expression
let string_of_structure structure = Format.asprintf "%a" pp_structure structure
