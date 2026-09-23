[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Tokens

type error = [ `lexer_error of string ]

val tokenize_lazy : string -> (token Seq.t, [> error ]) result
val tokenize : string -> (token list, [> error ]) result
