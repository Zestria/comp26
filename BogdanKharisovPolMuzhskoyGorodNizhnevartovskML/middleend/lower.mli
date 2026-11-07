[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  | Unbound_name of Bkml_frontend.Name.t
  | Duplicate_pattern_name of Bkml_frontend.Name.t
  | Integer_literal_out_of_range of Bkml_frontend.Integer_literal.t
  | Invalid_recursive_pattern
  | Identifier_error of Id.error

val error_to_string : error -> string

(** Resolve names, validate integer literals, compile irrefutable patterns and
    direct short-circuit primitive calls, and insert the unit branch of
    [if ... then ...]. Type inference and recursive-value initialization checks
    are separate responsibilities. The returned supply must be continued by
    later passes or translations whose results will be combined. *)
val expression
  :  Id.supply
  -> Bkml_frontend.Ast.expression
  -> (Lambda.expression * Id.supply, error) result

val program
  :  Id.supply
  -> Bkml_frontend.Ast.program
  -> (Lambda.program * Id.supply, error) result
