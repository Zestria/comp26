[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Interpret the unconverted functional language independently of its passes. *)
val observe : Bkml_middleend.Lambda.expression -> (string, Eval_value.error) result
