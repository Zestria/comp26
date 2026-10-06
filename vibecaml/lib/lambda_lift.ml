[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

exception Local_rec

let named name expression =
  { binding_pattern = Pat_var name; binding_expression = expression }
;;

(* Вынесенные определения одного объявления верхнего уровня копятся в
   обратном порядке. Тело функции обрабатывается до того, как она сама
   попадёт в список, поэтому вложенное оказывается раньше внешнего. Порядок
   обхода --- слева направо, так нумерация свежих имён предсказуема. *)
let lift_item (Structure_value (rec_flag, binding, bindings)) =
  let lifted = ref [] in
  let emit name expression = lifted := named name expression :: !lifted in
  let rec lift_expression = function
    | (Expr_constant _ | Expr_ident _) as expression -> expression
    | Expr_fun (parameter, parameters, body) ->
      let body = lift_expression body in
      let name = Fresh.name "lifted" in
      emit name (Expr_fun (parameter, parameters, body));
      Expr_ident name
    | Expr_let (Recursive, _, _, _) -> raise Local_rec
    | Expr_let (Non_recursive, binding, bindings, body) ->
      let kept = List.filter_map lift_local (binding :: bindings) in
      let body = lift_expression body in
      (match kept with
       | [] -> body
       | binding :: bindings -> Expr_let (Non_recursive, binding, bindings, body))
    | Expr_app (func, argument) ->
      let func = lift_expression func in
      let argument = lift_expression argument in
      Expr_app (func, argument)
    | Expr_if (condition, then_branch, else_branch) ->
      let condition = lift_expression condition in
      let then_branch = lift_expression then_branch in
      let else_branch = lift_expression else_branch in
      Expr_if (condition, then_branch, else_branch)
    | Expr_tuple (first, second, rest) ->
      let first = lift_expression first in
      let second = lift_expression second in
      let rest = List.map lift_expression rest in
      Expr_tuple (first, second, rest)
  (* Локальная именованная функция уходит наверх под своим именем. *)
  and lift_local = function
    | { binding_pattern = Pat_var name; binding_expression = Expr_fun (p, ps, body) } ->
      let body = lift_expression body in
      emit name (Expr_fun (p, ps, body));
      None
    | { binding_pattern; binding_expression } ->
      Some { binding_pattern; binding_expression = lift_expression binding_expression }
  in
  (* Именованная функция верхнего уровня остаётся на месте. *)
  let lift_top = function
    | { binding_pattern = Pat_var _ as binding_pattern
      ; binding_expression = Expr_fun (p, ps, body)
      } ->
      { binding_pattern; binding_expression = Expr_fun (p, ps, lift_expression body) }
    | { binding_pattern; binding_expression } ->
      { binding_pattern; binding_expression = lift_expression binding_expression }
  in
  let binding = lift_top binding in
  let bindings = List.map lift_top bindings in
  match rec_flag, List.rev !lifted with
  | Recursive, [] -> [ Structure_value (Recursive, binding, bindings) ]
  | Recursive, first :: rest ->
    [ Structure_value (Recursive, first, rest @ (binding :: bindings)) ]
  | Non_recursive, definitions ->
    let separate definition = Structure_value (Non_recursive, definition, []) in
    List.map separate definitions @ [ Structure_value (Non_recursive, binding, bindings) ]
;;

let lift program =
  try Ok (List.concat_map lift_item program) with
  | Local_rec -> Error "локальный let rec не поддерживается"
;;
