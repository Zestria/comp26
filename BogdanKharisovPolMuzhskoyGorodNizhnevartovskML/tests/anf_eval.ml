[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* All calls to converted code start with an environment made solely from the
   explicitly listed captures and arguments. No caller-scope fallback exists. *)
open Middleend
open Eval_value

let atom environment = function
  | Anf.Constant c -> constant c
  | Primitive p -> primitive p
  | Var id -> variable environment id
;;

let rec term function_ environment = function
  | Anf.Return value -> computation function_ environment value
  | Let (id, rhs, body) ->
    let* value = computation function_ environment rhs in
    term function_ (Id.Map.add id value environment) body
  | Let_rec (id, rhs, body) ->
    let* value =
      recursive (fun address -> term function_ (Id.Map.add id address environment) rhs)
    in
    term function_ (Id.Map.add id value environment) body
  | Sequence (first, second) ->
    let* _ = computation function_ environment first in
    term function_ environment second

and computation function_ environment = function
  | Anf.Atom value -> atom environment value
  | Tuple (a, b, rest) ->
    let* values = map (atom environment) (a :: b :: rest) in
    make (Tuple values)
  | Field (value, index) ->
    let* value = atom environment value in
    field value index
  | Apply (f, a, rest) ->
    let* arguments = map (atom environment) (a :: rest) in
    let* f = atom environment f in
    apply_arguments f arguments
  | If (condition, yes, no) ->
    let* condition = atom environment condition in
    let* condition = boolean condition in
    term function_ environment (if condition then yes else no)
  | Function definition -> function_ environment definition
;;

let bind_values error slots values =
  let rec bind environment slots values =
    match slots, values with
    | [], [] -> return environment
    | slot :: slots, value :: values ->
      bind (Id.Map.add slot value environment) slots values
    | [], _ :: _ | _ :: _, [] -> fail (Invalid_ir error)
  in
  bind Id.Map.empty slots values
;;

let environment outer slots captures =
  let* captures = map (atom outer) captures in
  bind_values (Capture_arity (List.length slots, List.length captures)) slots captures
;;

let closed expression =
  let rec function_ outer (definition : Closure.t) =
    let* local = environment outer definition.environment definition.captures in
    Eval_value.function_
      local
      (definition.parameter :: definition.parameters)
      (fun local -> term function_ local definition.body)
  in
  Eval_value.observe (fun () -> term function_ Id.Map.empty expression)
;;

let lifted (program : Lifted.program) =
  let declarations =
    List.fold_left
      (fun definitions (definition : Lifted.function_definition) ->
         Id.Map.add definition.code definition definitions)
      Id.Map.empty
      program.functions
  in
  let definition code =
    match Id.Map.find_opt code declarations with
    | Some definition -> return definition
    | None -> fail (Invalid_ir (Unknown_code code))
  in
  let rec function_ outer (closure : Lifted.closure) =
    let* definition = definition closure.code in
    let* local = environment outer definition.environment closure.captures in
    Eval_value.function_
      local
      (definition.parameter :: definition.parameters)
      (fun local -> term function_ local definition.body)
  in
  Eval_value.observe (fun () -> term function_ Id.Map.empty program.body)
;;
