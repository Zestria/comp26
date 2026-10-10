[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Top-down (combinator) parser. *)

type error = [ `Parsing_error of string ]

val pp_error : Format.formatter -> error -> unit

(** Parses an expression; keywords are reserved. *)
val parse : string -> (Ast.expr, error) result

(** Same as {!parse}, but keywords may also be used as identifiers
    (the [--extended] command line option). *)
val parse_extended : string -> (Ast.expr, error) result

(** Parses a program: declarations [let x = e] and [let rec f x = e] without
    [in], optionally preceded by an expression. *)
val parse_program : string -> (Ast.program, error) result

(** Same as {!parse_program}, with keywords allowed as identifiers. *)
val parse_program_extended : string -> (Ast.program, error) result
