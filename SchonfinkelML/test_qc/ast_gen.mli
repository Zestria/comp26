[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** QuickCheck generators for expressions and for parser input strings, shared
    by the parser tests and the benchmark generator. *)

open SchonfinkelML_frontend.Common

val binop_ops : string list
val names : string list
val gen_name : string QCheck.Gen.t
val gen_op : string QCheck.Gen.t
val gen_int : int QCheck.Gen.t
val gen_expr : int -> expr QCheck.Gen.t
val gen_expr_sized : expr QCheck.Gen.t
val table_words : string list
val gen_table_src : string QCheck.Gen.t
val lexer_words : string list
val gen_source : string QCheck.Gen.t
