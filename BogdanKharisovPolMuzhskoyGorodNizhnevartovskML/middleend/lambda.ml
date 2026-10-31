[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type expression =
  | Constant of Bkml_frontend.Constant.t
  | Var of Id.t
  | Primitive of Primitive.t
  | Tuple of expression * expression * expression list
  | Field of expression * int
  | Fun of function_definition
  | Apply of expression * expression * expression list
  | Let of Id.t * expression * expression
  | Let_rec of Id.t * expression * expression
  | If of expression * expression * expression
  | Sequence of expression * expression

and function_definition =
  { parameter : Id.t
  ; parameters : Id.t list
  ; body : expression
  }

type program = expression

let rec free_variables = function
  | Constant _ | Primitive _ -> Id.Set.empty
  | Var id -> Id.Set.singleton id
  | Tuple (a, b, rest) -> free_variables_list (a :: b :: rest)
  | Field (value, _) -> free_variables value
  | Fun { parameter; parameters; body } ->
    List.fold_left
      (fun free id -> Id.Set.remove id free)
      (free_variables body)
      (parameter :: parameters)
  | Apply (f, a, rest) -> free_variables_list (f :: a :: rest)
  | Let (id, value, body) ->
    Id.Set.union (free_variables value) (Id.Set.remove id (free_variables body))
  | Let_rec (id, value, body) -> Id.Set.remove id (free_variables_list [ value; body ])
  | If (condition, yes, no) -> free_variables_list [ condition; yes; no ]
  | Sequence (first, second) -> free_variables_list [ first; second ]

and free_variables_list expressions =
  List.fold_left
    (fun free expression -> Id.Set.union free (free_variables expression))
    Id.Set.empty
    expressions
;;

type invariant_error =
  | Unbound_variable of Id.t
  | Duplicate_binding of Id.t
  | Negative_field of int

let invariant_error_to_string = function
  | Unbound_variable id -> "unbound identifier " ^ Id.name id
  | Duplicate_binding id -> "duplicate binding " ^ Id.name id
  | Negative_field index -> "negative field index " ^ string_of_int index
;;

let validate ?(globals = Id.Set.empty) expression =
  let ( let* ) = Result.bind in
  let declare bound seen id =
    if Id.Set.mem id seen
    then Error (Duplicate_binding id)
    else Ok (Id.Set.add id bound, Id.Set.add id seen)
  in
  let rec check bound seen = function
    | Constant _ | Primitive _ -> Ok seen
    | Var id when not (Id.Set.mem id bound) -> Error (Unbound_variable id)
    | Var _ -> Ok seen
    | Tuple (a, b, rest) -> check_list bound seen (a :: b :: rest)
    | Field (_, index) when index < 0 -> Error (Negative_field index)
    | Field (value, _) -> check bound seen value
    | Fun { parameter; parameters; body } ->
      let* bound, seen =
        List.fold_left
          (fun state id ->
             let* bound, seen = state in
             declare bound seen id)
          (Ok (bound, seen))
          (parameter :: parameters)
      in
      check bound seen body
    | Apply (f, a, rest) -> check_list bound seen (f :: a :: rest)
    | Let (id, value, body) ->
      let* inner, seen = declare bound seen id in
      let* seen = check bound seen value in
      check inner seen body
    | Let_rec (id, value, body) ->
      let* bound, seen = declare bound seen id in
      check_list bound seen [ value; body ]
    | If (condition, yes, no) -> check_list bound seen [ condition; yes; no ]
    | Sequence (first, second) -> check_list bound seen [ first; second ]
  and check_list bound seen = function
    | [] -> Ok seen
    | expression :: rest ->
      let* seen = check bound seen expression in
      check_list bound seen rest
  in
  Result.map (fun _ -> ()) (check globals globals expression)
;;
