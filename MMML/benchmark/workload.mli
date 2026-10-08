[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Deterministic inputs for parser benchmarks. *)

type t =
  { definitions : int
  ; program : Ast.program
  ; source : string
  }

(** [create definitions] builds a program containing exactly [definitions]
    top-level bindings. It generates a bounded AST covering every current
    expression and pattern constructor, then renders it as source code. The
    generated program is deterministic. *)
val create : int -> t

(** Default workload sizes, measured in top-level definitions. *)
val default_sizes : int list
