[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Top-down (recursive descent) parser over the token stream produced by [Frontend.Lexer].

    Operators are described by an [Operators.t], so their arity, associativity and
    precedence are data: the table can be redefined from the command line.

    The source is read token by token from a lexbuf, and what [Frontend.Lexer]
    rejects comes back as an [error] rather than as an exception. *)

(** What went wrong: a position and a message, as the lexer has it. *)
type error = Frontend.Lexer.error

(** Errors print as [line:column: message], whether they came from lexing or
    from parsing; [Frontend.Lexer.pp_error] says more about the format. *)
val pp_error : Format.formatter -> error -> unit

val string_of_error : error -> string

(** Parse one expression, allowing only whitespace and comments after it.
    Operators are taken from [operators], [Operators.default] by default.

    The source is read token by token as parsing goes, so a lexing error is
    reported only when the lexer reaches it: an error the parser finds earlier
    in the source comes first.

    [budget] bounds how many parser frames may be alive at once, and so the
    nesting the parser accepts; it is spent one unit per nested call and is
    [100_000] by default.  Going over it, or overflowing the stack, is reported
    as an error rather than crashing. *)
val parse_expr
  :  ?ops:Operators.t
  -> ?max_budget:int
  -> string
  -> (Frontend.Common.expr, error) result
