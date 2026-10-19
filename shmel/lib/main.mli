[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Parser entry points shared by the CLI, tests and benchmark. *)

(** Bottom-up (menhir) parser. Raises [Parser.Error] on a syntax error and
    [Failure] on a lexing error. *)
val parse_with_menhir : string -> Ast.expr

(** Same as {!parse_with_menhir}, but returns errors with their position. *)
val parse_with_menhir_result : string -> (Ast.expr, [ `Parsing_error of string ]) result

(** Top-down (combinator) parser. *)
val parse_with_comb : string -> (Ast.expr, Parser_comb.error) result

(** Top-down parser with keywords allowed as identifiers ([--extended]). *)
val parse_with_comb_extended : string -> (Ast.expr, Parser_comb.error) result

(** Top-down parser for a whole program (see {!Parser_comb.parse_program}). *)
val parse_program_with_comb : string -> (Ast.program, Parser_comb.error) result

(** Same as {!parse_program_with_comb}, with keywords allowed as identifiers. *)
val parse_program_with_comb_extended : string -> (Ast.program, Parser_comb.error) result

(** The menhir grammar does not support declarations yet, so the program is a
    single expression. *)
val parse_program_with_menhir
  :  string
  -> (Ast.program, [ `Parsing_error of string ]) result
