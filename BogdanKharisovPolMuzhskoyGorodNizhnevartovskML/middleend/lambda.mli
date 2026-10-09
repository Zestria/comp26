[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Untyped functional IR with resolved binding identities and no patterns. *)
type expression =
  | Constant of Frontend.Constant.t
  | Var of Id.t
  | Primitive of Primitive.t
  (** Primitive function values are strict, including [And] and [Or]. Direct
      short-circuit calls are lowered to [If]. *)
  | Tuple of expression * expression * expression list
  | Field of expression * int
  (** Zero-based projection. Typing checks the tuple type and its arity. *)
  | Fun of function_definition
  | Apply of expression * expression * expression list
  (** Application groups are preserved. Arguments are evaluated right to left,
      then the callee. *)
  | Let of Id.t * expression * expression
  | Let_rec of Id.t * expression * expression
  (** General recursive values; safe initialization is checked separately. *)
  | If of expression * expression * expression
  | Sequence of expression * expression

and function_definition =
  { parameter : Id.t
  ; parameters : Id.t list
  ; body : expression
  }

(** Structure items run in order and the program returns unit. *)
type program = expression

val free_variables : expression -> Id.Set.t

type invariant_error =
  | Unbound_variable of Id.t
  | Duplicate_binding of Id.t
  | Negative_field of int

val invariant_error_to_string : invariant_error -> string

(** Check scope, globally unique binding occurrences and nonnegative field
    indices. [globals] are existing bindings visible at entry. No type or
    recursive-initialization checking is performed. *)
val validate : ?globals:Id.Set.t -> expression -> (unit, invariant_error) result
