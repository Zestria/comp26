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
   обхода --- слева направо, так нумерация свежих имён предсказуема. *)
let lift_item (Structure_value (rec_flag, binding, bindings)) =
  let lifted = ref [] in
  let emit name expression = lifted := Single (named name expression) :: !lifted in
  let rec lift_expression = function
    | (Expr_constant _ | Expr_ident _) as expression -> expression
    | Expr_fun (parameter, parameters, body) ->
      let func = lift_function parameter parameters body in
      let name = Fresh.name "lifted" in
      emit name func;
      Expr_ident name
    | Expr_let (Recursive, binding, bindings, body) ->
      lift_group binding bindings;
      lift_expression body
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
  and lift_function parameter parameters body =
    Expr_fun (parameter, parameters, lift_expression body)
  (* Всё вынесенное из тел группы попадает в тот же let rec: такие функции
     могут вызывать группу. Какие именно вызывают, не выясняем --- лишний
     член let rec безвреден. *)
  and lift_group binding bindings =
    let outer = !lifted in
    lifted := [];
    let lift_member = function
      | { binding_pattern = Pat_var name
        ; binding_expression = Expr_fun (parameter, parameters, body)
        } -> named name (lift_function parameter parameters body)
      | _ -> raise (Unsupported "в let rec справа должна стоять функция")
    in
    let binding = lift_member binding in
    let bindings = List.map lift_member bindings in
    let inner = List.concat_map bindings_of (List.rev !lifted) in
    lifted := Group (binding, inner @ bindings) :: outer
  and lift_local = function
    | { binding_pattern = Pat_var name
      ; binding_expression = Expr_fun (parameter, parameters, body)
      } ->
      emit name (lift_function parameter parameters body);
      None
    | { binding_pattern; binding_expression } ->
      Some { binding_pattern; binding_expression = lift_expression binding_expression }
  in
  (* В let rec OCaml разрешает справа только функцию, а после выноса
     значение превратилось бы в применение функции из той же группы. *)
  let lift_top = function
    | { binding_pattern = Pat_var _ as binding_pattern
      ; binding_expression = Expr_fun (parameter, parameters, body)
      } ->
      { binding_pattern; binding_expression = lift_function parameter parameters body }
    | _ when rec_flag = Recursive ->
      raise (Unsupported "в let rec справа должна стоять функция")
    | { binding_pattern; binding_expression } ->
      { binding_pattern; binding_expression = lift_expression binding_expression }
  in
  let binding = lift_top binding in
  let bindings = List.map lift_top bindings in
  match rec_flag, List.rev !lifted with
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
