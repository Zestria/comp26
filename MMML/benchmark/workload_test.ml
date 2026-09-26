[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type coverage =
  { integer : bool
  ; boolean : bool
  ; unit_ : bool
  ; variable : bool
  ; lambda : bool
  ; application : bool
  ; conditional : bool
  ; nonrecursive_let : bool
  ; recursive_let : bool
  ; wildcard_pattern : bool
  ; variable_pattern : bool
  }

let empty =
  { integer = false
  ; boolean = false
  ; unit_ = false
  ; variable = false
  ; lambda = false
  ; application = false
  ; conditional = false
  ; nonrecursive_let = false
  ; recursive_let = false
  ; wildcard_pattern = false
  ; variable_pattern = false
  }
;;

let cover_pattern coverage = function
  | PWildcard -> { coverage with wildcard_pattern = true }
  | PVariable _ -> { coverage with variable_pattern = true }
;;

let rec cover_expression coverage = function
  | Constant (Integer _) -> { coverage with integer = true }
  | Constant (Boolean _) -> { coverage with boolean = true }
  | Constant Unit -> { coverage with unit_ = true }
  | Variable _ -> { coverage with variable = true }
  | Lambda (parameter, body) ->
    let coverage = cover_pattern { coverage with lambda = true } parameter in
    cover_expression coverage body
  | Application (function_, argument) ->
    let coverage = { coverage with application = true } in
    let coverage = cover_expression coverage function_ in
    cover_expression coverage argument
  | If_then_else (condition, if_true, if_false) ->
    let coverage = { coverage with conditional = true } in
    let coverage = cover_expression coverage condition in
    let coverage = cover_expression coverage if_true in
    cover_expression coverage if_false
  | Let_in (rec_flag, binding, value, body) ->
    let coverage =
      match rec_flag with
      | Nonrecursive -> { coverage with nonrecursive_let = true }
      | Recursive -> { coverage with recursive_let = true }
    in
    let coverage = cover_pattern coverage binding in
    let coverage = cover_expression coverage value in
    cover_expression coverage body
;;

let cover_definition coverage (Definition (_, binding, value)) =
  cover_expression (cover_pattern coverage binding) value
;;

let complete coverage =
  coverage.integer
  && coverage.boolean
  && coverage.unit_
  && coverage.variable
  && coverage.lambda
  && coverage.application
  && coverage.conditional
  && coverage.nonrecursive_let
  && coverage.recursive_let
  && coverage.wildcard_pattern
  && coverage.variable_pattern
;;

let () =
  let first = Workload.create 200 in
  let second = Workload.create 200 in
  if first <> second then failwith "workload generation is not deterministic";
  if List.length first.program <> first.definitions
  then failwith "workload contains an unexpected number of definitions";
  let coverage = List.fold_left cover_definition empty first.program in
  if not (complete coverage) then failwith "workload does not cover every AST constructor"
;;
