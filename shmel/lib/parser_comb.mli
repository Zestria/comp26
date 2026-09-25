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
