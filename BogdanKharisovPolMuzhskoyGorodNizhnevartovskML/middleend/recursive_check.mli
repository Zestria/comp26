[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error = Unsafe_recursive_definition of Frontend.Name.t

val error_to_string : error -> string

(** Check initialization before source patterns are erased. Name resolution
    and duplicate/recursive pattern checks belong to [Lower]. Type inference
    is separate; this analysis does not reject ill-typed expressions as such. *)
val expression : Frontend.Ast.expression -> (unit, error) result

val program : Frontend.Ast.program -> (unit, error) result
