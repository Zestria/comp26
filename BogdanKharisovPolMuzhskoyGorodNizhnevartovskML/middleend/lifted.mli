[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Function values reference program-level code and supply its environment. *)
type closure =
  { code : Id.t
  ; captures : Anf.atom list
  }

type expression = closure Anf.term

type function_definition =
  { code : Id.t
  ; environment : Id.t list
  ; parameter : Id.t
  ; parameters : Id.t list
  ; body : expression
  }

type program =
  { functions : function_definition list
  ; body : expression
  }

type error =
  | Binding of Lambda.invariant_error
  | Duplicate_code of Id.t
  | Unknown_code of Id.t
  | Capture_count of Id.t * int * int

val error_to_string : error -> string

(** Check unique code labels and binding identities, lexical scope, field
    indices, and capture counts. Each code body sees only its own environment
    slots and parameters; it cannot use bindings from the program body. *)
val validate : program -> (unit, error) result
