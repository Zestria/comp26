[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Interpreter for Lambda expressions. *)
open Middleend
open Eval_value

let rec evaluate environment = function
  | Lambda.Constant c -> constant c
  | Var id -> variable environment id
  | Primitive p -> primitive p
  | Tuple (a, b, rest) ->
    let* values = evaluate_arguments environment (a :: b :: rest) in
    make (Tuple values)
  | Field (tuple, index) ->
    let* tuple = evaluate environment tuple in
    field tuple index
  | Fun { parameter; parameters; body } ->
    function_ environment (parameter :: parameters) (fun environment ->
      evaluate environment body)
  | Apply (f, a, rest) ->
    let* arguments = evaluate_arguments environment (a :: rest) in
    let* f = evaluate environment f in
    apply_arguments f arguments
  | Let (id, value, body) ->
    let* value = evaluate environment value in
    evaluate (Id.Map.add id value environment) body
  | Let_rec (id, rhs, body) ->
    let* value =
      recursive (fun address -> evaluate (Id.Map.add id address environment) rhs)
    in
    evaluate (Id.Map.add id value environment) body
  | If (condition, yes, no) ->
    let* condition = evaluate environment condition in
    let* condition = boolean condition in
    evaluate environment (if condition then yes else no)
  | Sequence (first, second) ->
    let* _ = evaluate environment first in
    evaluate environment second

and evaluate_arguments environment = function
  | [] -> return []
  | first :: rest ->
    let* rest = evaluate_arguments environment rest in
    let* first = evaluate environment first in
    return (first :: rest)
;;

let run = evaluate Id.Map.empty
let observe expression = Eval_value.observe (fun () -> run expression)
