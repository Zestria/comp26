[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** The bottom-up parser, generated from [parser_gen.mly] with Menhir. *)

(** A lexing error.  [parse_expr] catches it and reports [Error ()], so the
    type is exposed only to name what the lexer can raise. *)
type error = SchonfinkelML_frontend.Lexer.error

(** [parse_expr src] parses exactly one expression from [src].  Any lexing or
    syntax error, as well as a source that is not a single expression, is
    reported as [Error ()]. *)
val parse_expr : string -> (SchonfinkelML_frontend.Common.expr, unit) result
