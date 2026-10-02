[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Ast

let pp_constant ppf = function
  | CInt n when n < 0 -> fprintf ppf "(%d)" n
  | CInt n -> fprintf ppf "%d" n
  | CBool b -> fprintf ppf "%b" b
  | CUnit -> fprintf ppf "()"
;;

let pp_rec_flag ppf = function
  | Rec -> fprintf ppf " rec"
  | NonRec -> fprintf ppf ""
;;

let pp_ident ppf name =
  if Syntax.is_operator Syntax.default_table name
  then fprintf ppf "(%s)" name
  else fprintf ppf "%s" name
;;

let pp_comma_separated pp_element =
  pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_element
;;

let pp_pattern ppf = function
  | PAny -> fprintf ppf "_"
  | PVar name -> pp_ident ppf name
  | PConstant constant -> fprintf ppf "%a" pp_constant constant
  | PTuple (first, second, rest) ->
    let separator = pp_comma_separated pp_pattern in
    fprintf ppf "(%a)" separator (first :: second :: rest)
;;

let rec pp_expression ppf = function
  | EConst constant -> fprintf ppf "%a" pp_constant constant
  | EIdent name -> pp_ident ppf name
  | EFun (parameter, body) ->
    fprintf ppf "fun %a -> %a" pp_pattern parameter pp_expression body
  | EFunction (first, rest) -> fprintf ppf "function %a" pp_cases (first, rest)
  | EMatch (subject, first, rest) ->
    fprintf ppf "match %a with %a" pp_atom subject pp_cases (first, rest)
  | EIf (condition, then_branch, None) ->
    fprintf ppf "if %a then %a" pp_atom condition pp_atom then_branch
  | EIf (condition, then_branch, Some else_branch) ->
    fprintf
      ppf
      "if %a then %a else %a"
      pp_atom
      condition
      pp_atom
      then_branch
      pp_atom
      else_branch
  | ELet (rec_flag, first, rest, body) ->
    fprintf
      ppf
      "let%a %a in %a"
      pp_rec_flag
      rec_flag
      pp_value_bindings
      (first, rest)
      pp_expression
      body
  | ETuple (first, second, rest) ->
    fprintf ppf "(%a)" (pp_comma_separated pp_atom) (first :: second :: rest)
  | EApply _ as application -> pp_application ppf application

and pp_application ppf = function
  | EApply (EApply (EIdent name, left), right)
    when Syntax.lookup_infix Syntax.default_table name <> None ->
    fprintf ppf "%a %s %a" pp_atom left name pp_atom right
  | EApply (EIdent name, argument)
    when Syntax.lookup_prefix Syntax.default_table name <> None ->
    fprintf ppf "%s %a" name pp_atom argument
  | EApply (fn, argument) -> fprintf ppf "%a %a" pp_head fn pp_atom argument
  | atom -> fprintf ppf "%a" pp_atom atom

and pp_head ppf = function
  | EIdent name -> pp_ident ppf name
  | head -> fprintf ppf "(%a)" pp_expression head

and pp_atom ppf = function
  | (EConst _ | EIdent _ | ETuple _) as atom -> fprintf ppf "%a" pp_expression atom
  | atom -> fprintf ppf "(%a)" pp_expression atom

and pp_value_bindings ppf (first, rest) =
  List.iteri
    (fun index { vb_pat; vb_expr } ->
       if index > 0 then fprintf ppf " and ";
       fprintf ppf "%a = %a" pp_pattern vb_pat pp_atom vb_expr)
    (first :: rest)

and pp_case_body ppf case_expr =
  let rec ends_with_case = function
    | EFun _ | EMatch _ | EFunction _ -> true
    | ELet (_, _, _, body) -> ends_with_case body
    | EIf (_, _, Some branch) -> ends_with_case branch
    | _ -> false
  in
  if ends_with_case case_expr
  then fprintf ppf "(%a)" pp_expression case_expr
  else fprintf ppf "%a" pp_expression case_expr

and pp_case ppf { case_pat; case_expr } =
  fprintf ppf "| %a -> %a" pp_pattern case_pat pp_case_body case_expr

and pp_cases ppf (first, rest) =
  List.iteri
    (fun index case ->
       if index > 0 then fprintf ppf "\n";
       fprintf ppf "%a" pp_case case)
    (first :: rest)
;;

let pp_structure_item ppf = function
  | SEval expression -> fprintf ppf "%a;;" pp_expression expression
  | SValue (rec_flag, first, rest) ->
    fprintf ppf "let%a %a;;" pp_rec_flag rec_flag pp_value_bindings (first, rest)
;;

let pp_structure ppf items =
  List.iteri
    (fun index item ->
       if index > 0 then fprintf ppf "\n";
       fprintf ppf "%a" pp_structure_item item)
    items
;;

let string_of_expression expression = asprintf "%a" pp_expression expression
let string_of_structure structure = asprintf "%a" pp_structure structure
