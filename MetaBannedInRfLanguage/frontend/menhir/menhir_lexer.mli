[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** ocamllex lexer producing the tokens of the menhir grammar. *)

(** Raised on a malformed token, with a human-readable description. *)
exception Error of string

(** Reads the next token from the buffer. *)
val token : Lexing.lexbuf -> Menhir_grammar.token
