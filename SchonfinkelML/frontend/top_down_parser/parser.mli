[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Recursive descent parser for a subset of OCaml, without a separate lexer.

    The parser decides lexeme boundaries itself, so a keyword may stick to an
    identifier: with [glued:true], [letx=1inx] is [let x = 1 in x]; with
    [glued:false] keywords need a token boundary ([letx] is one identifier).

    Never exits: any input yields [Ok] or [Error] with a position.

    Out of scope: [()], [true]/[false], float, string, [match], toplevel
    bindings, [and], []. *)

type error = Cursor.error =
  { pos : Lexing.position
  ; msg : string
  }

val pp_error : Format.formatter -> error -> unit
val string_of_error : error -> string

(** Parse one expression, allowing only whitespace and comments after it.
    [glued] enables glued keywords. *)
val parse_expr : ?glued:bool -> string -> (Frontend.Common.expr, error) result
