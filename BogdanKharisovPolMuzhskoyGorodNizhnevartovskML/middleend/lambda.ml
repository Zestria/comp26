[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Functional intermediate language. Patterns have been compiled away;
    binding identity is independent of source names. This is not a typed IR. *)
type expression =
  | Constant of Frontend.Constant.t
  | Var of Id.t
  | Primitive of Primitive.t
  (** A first-class primitive function. Application is strict, including for
      [And] and [Or]; direct short-circuit calls are lowered to [If]. *)
  | Tuple of expression * expression * expression list
  | Field of expression * int
  (** Zero-based projection; the input's tuple type is checked by typing. *)
  | Fun of function_definition
  | Apply of expression * expression * expression list
  (** Preserve application groups. Arguments are evaluated right to left,
      then the callee. *)
  | Let of Id.t * expression * expression
  | Let_rec of Id.t * expression * expression
  (** General recursive values are retained. Their initialization restrictions
      are not a name-resolution check. *)
  | If of expression * expression * expression
  | Sequence of expression * expression

and function_definition =
  { parameter : Id.t
  ; parameters : Id.t list
  ; body : expression
  }

(** A standalone program runs its structure items in order and returns unit. *)
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

(** Check lexical scope and uniqueness of binding occurrences over the entire
    term, including disjoint branches. Does not check types or recursive-value
    initialization. Moving a binder preserves its Id; copying it requires a
    fresh Id and corresponding renaming of its uses. *)
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
