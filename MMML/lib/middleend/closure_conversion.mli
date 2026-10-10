[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type problem =
  | Unbound_identifier of string
  | Recursive_wildcard
  | Recursive_rhs_not_lambda of string
  | Missing_recursive_code of string

type error =
  { path : string list
  ; problem : problem
  }

(** Each consecutive lambda group receives its free local names as fresh
    leading parameters, ordered by the original name. Function values are
    formed by partial application. Groups never cross let/if/application.
    Self references also use partial application in value positions.

    Source bindings and public top-level names are retained. Generated names
    avoid all source names and declared externals. Repeated calls and external
    list order/duplicates do not change the result. The input is not mutated.

    Recursive bindings require a named lambda RHS. Errors carry structural
    paths, not fabricated source locations. External names must be declared;
    primitive semantics belong to later passes/runtime. *)
val convert : externals:string list -> Ast.program -> (Ast.program, error) result

val pp_error : Format.formatter -> error -> unit
