[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Unambiguous rendering of the surface AST as MiniML source. *)

open Ast

let parenthesize condition printer formatter value =
  if condition
  then Format.fprintf formatter "(@[%a@])" printer value
  else printer formatter value
;;

let pp_constant formatter = function
  | Integer value -> Format.pp_print_int formatter value
  | Boolean value -> Format.pp_print_bool formatter value
  | Unit -> Format.pp_print_string formatter "()"
;;

let is_operator_name name =
  String.length name > 0
  &&
  match name.[0] with
  | '!'
  | '$'
  | '%'
  | '&'
  | '*'
  | '+'
  | '-'
  | '.'
  | '/'
  | ':'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '^'
  | '|'
  | '~' -> true
  | _ -> false
;;

let pp_identifier formatter name =
  if is_operator_name name
  then Format.fprintf formatter "(%s)" name
  else Format.pp_print_string formatter name
;;

let pp_pattern formatter = function
  | PWildcard -> Format.pp_print_string formatter "_"
  | PVariable name -> pp_identifier formatter name
;;

let rec flatten_applications arguments = function
  | Application (function_, argument) ->
    flatten_applications (argument :: arguments) function_
  | function_ -> function_, arguments
;;

let rec flatten_lambdas parameters = function
  | Lambda (parameter, body) -> flatten_lambdas (parameter :: parameters) body
  | body -> List.rev parameters, body
;;

let rec pp_expr_at nested formatter expression =
  match expression with
  | Application (Application (Variable operator, left), right) as application ->
    if is_operator_name operator
    then pp_infix nested formatter operator left right
    else pp_application nested formatter application
  | Constant (Integer value) when value < 0 ->
    parenthesize nested Format.pp_print_int formatter value
  | Constant constant -> pp_constant formatter constant
  | Variable name -> pp_identifier formatter name
  | Lambda _ -> pp_lambda nested formatter expression
  | Application _ -> pp_application nested formatter expression
  | If_then_else (condition, if_true, if_false) ->
    parenthesize
      nested
      (fun formatter (condition, if_true, if_false) ->
         Format.fprintf
           formatter
           "@[<v 0>if %a then@;<1 2>%a@;else@;<1 2>%a@]"
           (pp_expr_at false)
           condition
           (pp_expr_at false)
           if_true
           (pp_expr_at false)
           if_false)
      formatter
      (condition, if_true, if_false)
  | Let_in (recursive, pattern, value, body) ->
    parenthesize
      nested
      (fun formatter () ->
         Format.fprintf
           formatter
           "@[<v 0>%a@,in@;<1 2>%a@]"
           (pp_binding ("let" ^ pp_rec_flag recursive))
           (pattern, value)
           (pp_expr_at false)
           body)
      formatter
      ()

and pp_infix nested formatter operator left right =
  parenthesize
    nested
    (fun formatter (left, right) ->
       Format.fprintf
         formatter
         "@[<hov 2>%a %s@ %a@]"
         (pp_expr_at true)
         left
         operator
         (pp_expr_at true)
         right)
    formatter
    (left, right)

and pp_application nested formatter expression =
  let function_, arguments = flatten_applications [] expression in
  parenthesize
    nested
    (fun formatter () ->
       Format.fprintf formatter "@[<hov 2>%a" (pp_expr_at true) function_;
       List.iter (Format.fprintf formatter "@ %a" (pp_expr_at true)) arguments;
       Format.fprintf formatter "@]")
    formatter
    ()

and pp_lambda nested formatter expression =
  let parameters, body = flatten_lambdas [] expression in
  parenthesize
    nested
    (fun formatter () ->
       Format.fprintf formatter "@[<hov 2>fun";
       List.iter (Format.fprintf formatter "@ %a" pp_pattern) parameters;
       Format.fprintf formatter "@ ->@ %a@]" (pp_expr_at false) body)
    formatter
    ()

and pp_binding keyword formatter (pattern, value) =
  match pattern, flatten_lambdas [] value with
  | PVariable name, ((_ :: _ as parameters), body) ->
    Format.fprintf formatter "@[<hov 2>%s %a" keyword pp_identifier name;
    List.iter (Format.fprintf formatter "@ %a" pp_pattern) parameters;
    Format.fprintf formatter "@ =@;<1000 0>%a@]" (pp_expr_at false) body
  | _ ->
    Format.fprintf
      formatter
      "@[<hov 2>%s %a =@;<1000 0>%a@]"
      keyword
      pp_pattern
      pattern
      (pp_expr_at false)
      value

and pp_rec_flag = function
  | Nonrecursive -> ""
  | Recursive -> " rec"
;;

let pp_expr = pp_expr_at false

let pp_definition formatter (Definition (recursive, pattern, value)) =
  pp_binding ("let" ^ pp_rec_flag recursive) formatter (pattern, value)
;;

let pp_program formatter =
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,@,")
    pp_definition
    formatter
;;

let expr_to_string expression = Format.asprintf "%a" pp_expr expression
let program_to_string program = Format.asprintf "%a" pp_program program
