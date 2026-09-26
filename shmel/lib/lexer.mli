[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Lexer for the bottom-up (menhir) parser. Raises [Failure] on an
    unexpected character. *)
val read : Lexing.lexbuf -> Parser.token
