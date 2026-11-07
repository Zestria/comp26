[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

(* Ловится на границе модуля в [lift] и превращается в [Error]. *)
exception Unsupported of string

let named name expression =
  { binding_pattern = Pat_var name; binding_expression = expression }
;;

(* Вынесенное определение: одиночная функция или локальная рекурсивная
   группа вместе со всем, что вынесено из её тел. *)
type lifted =
  | Single of value_binding
  | Group of value_binding * value_binding list

let bindings_of = function
  | Single binding -> [ binding ]
  | Group (binding, bindings) -> binding :: bindings
;;

(* Вынесенные определения одного объявления верхнего уровня копятся в
   обратном порядке. Тело функции обрабатывается до того, как она сама
   попадёт в список, поэтому вложенное оказывается раньше внешнего. Порядок
   обхода --- слева направо, так нумерация свежих имён предсказуема. Накопитель
   идёт первым в паре результата: такую функцию ждёт [List.fold_left_map]. *)
let emit lifted name expression = Single (named name expression) :: lifted

let rec lift_expression lifted = function
  | (Expr_constant _ | Expr_ident _) as expression -> lifted, expression
  | Expr_fun (parameter, parameters, body) ->
    let lifted, func = lift_function lifted parameter parameters body in
    let name = Fresh.name "lifted" in
    let lifted = emit lifted name func in
    lifted, Expr_ident name
  | Expr_let (Recursive, binding, bindings, body) ->
    let lifted = lift_group lifted binding bindings in
    lift_expression lifted body
  | Expr_let (Non_recursive, binding, bindings, body) ->
    let lifted, kept = List.fold_left_map lift_local lifted (binding :: bindings) in
    let lifted, body = lift_expression lifted body in
    (match List.filter_map Fun.id kept with
     | [] -> lifted, body
     | binding :: bindings -> lifted, Expr_let (Non_recursive, binding, bindings, body))
  | Expr_app (func, argument) ->
    let lifted, func = lift_expression lifted func in
    let lifted, argument = lift_expression lifted argument in
    lifted, Expr_app (func, argument)
  | Expr_if (condition, then_branch, else_branch) ->
    let lifted, condition = lift_expression lifted condition in
    let lifted, then_branch = lift_expression lifted then_branch in
    let lifted, else_branch = lift_expression lifted else_branch in
    lifted, Expr_if (condition, then_branch, else_branch)
  | Expr_tuple (first, second, rest) ->
    let lifted, first = lift_expression lifted first in
    let lifted, second = lift_expression lifted second in
    let lifted, rest = List.fold_left_map lift_expression lifted rest in
    lifted, Expr_tuple (first, second, rest)

and lift_function lifted parameter parameters body =
  let lifted, body = lift_expression lifted body in
  lifted, Expr_fun (parameter, parameters, body)

(* Всё вынесенное из тел группы попадает в тот же let rec: такие функции
   могут вызывать группу. Какие именно вызывают, не выясняем --- лишний
   член let rec безвреден. *)
and lift_group outer binding bindings =
  let lift_member lifted = function
    | { binding_pattern = Pat_var name
      ; binding_expression = Expr_fun (parameter, parameters, body)
      } ->
      let lifted, func = lift_function lifted parameter parameters body in
      lifted, named name func
    | _ -> raise (Unsupported "в let rec справа должна стоять функция")
  in
  let lifted, binding = lift_member [] binding in
  let lifted, bindings = List.fold_left_map lift_member lifted bindings in
  let inner = List.concat_map bindings_of (List.rev lifted) in
  Group (binding, inner @ bindings) :: outer

and lift_local lifted = function
  | { binding_pattern = Pat_var name
    ; binding_expression = Expr_fun (parameter, parameters, body)
    } ->
    let lifted, func = lift_function lifted parameter parameters body in
    emit lifted name func, None
  | { binding_pattern; binding_expression } ->
    let lifted, binding_expression = lift_expression lifted binding_expression in
    lifted, Some { binding_pattern; binding_expression }
;;

let lift_item (Structure_value (rec_flag, binding, bindings)) =
  (* В let rec OCaml разрешает справа только функцию, а после выноса
     значение превратилось бы в применение функции из той же группы. *)
  let lift_top lifted = function
    | { binding_pattern = Pat_var _ as binding_pattern
      ; binding_expression = Expr_fun (parameter, parameters, body)
      } ->
      let lifted, binding_expression = lift_function lifted parameter parameters body in
      lifted, { binding_pattern; binding_expression }
    | _ when rec_flag = Recursive ->
      raise (Unsupported "в let rec справа должна стоять функция")
    | { binding_pattern; binding_expression } ->
      let lifted, binding_expression = lift_expression lifted binding_expression in
      lifted, { binding_pattern; binding_expression }
  in
  let lifted, binding = lift_top [] binding in
  let lifted, bindings = List.fold_left_map lift_top lifted bindings in
  match rec_flag, List.rev lifted with
  | Recursive, definitions ->
    (match List.concat_map bindings_of definitions with
     | [] -> [ Structure_value (Recursive, binding, bindings) ]
     | first :: rest ->
       [ Structure_value (Recursive, first, rest @ (binding :: bindings)) ])
  | Non_recursive, definitions ->
    let separate = function
      | Single definition -> Structure_value (Non_recursive, definition, [])
      | Group (first, rest) -> Structure_value (Recursive, first, rest)
    in
    List.map separate definitions @ [ Structure_value (Non_recursive, binding, bindings) ]
;;

let lift program =
  try Ok (List.concat_map lift_item program) with
  | Unsupported message -> Error message
;;
