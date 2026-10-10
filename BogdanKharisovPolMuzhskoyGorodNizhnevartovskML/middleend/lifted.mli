[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Function values reference program-level code and supply its environment. *)
type closure =
  { code : Id.t
  ; captures : Anf.atom list
  }

(** Direct calls name closed code and supply exactly its declared arity.
    Partial and overapplications still use an ordinary function value. *)
type callee =
  | Indirect of Anf.atom
  | Direct of Id.t

type expression = (closure, callee) Anf.term

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
  | Direct_environment of Id.t
  | Call_arity of Id.t * int * int

val error_to_string : error -> string

(** Check unique code labels and binding identities, lexical scope, field
    indices, capture counts and the direct-call contract. Each code body sees
    only its own environment slots and parameters; it cannot use bindings from
    the program body. *)
val validate : program -> (unit, error) result
