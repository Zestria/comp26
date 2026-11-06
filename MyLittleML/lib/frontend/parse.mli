[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Unified parsing API for MyLittleML (Task 1).

    Both parsers consume a source string and produce the same {!Ast.prog}
    or a ["line:col: message"] error. *)

type parser_kind =
  | Topdown
  | Bottomup

type opts = { sticky : bool }

val parse : parser_kind -> opts -> string -> (Ast.prog, string) result
