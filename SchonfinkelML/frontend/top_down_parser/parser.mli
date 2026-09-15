[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Top-down (recursive descent) parser over the token stream produced by [Frontend.Lexer].

    Operators are described by an [Operators.t], so their arity, associativity and
    precedence are data: the table can be redefined from the command line.

    Reading the source is done by [tokenize], which is here as well: it pairs
    every token of [Frontend.Lexer] with the position it starts at. *)

type error =
  { pos : Lexing.position
  ; msg : string
  }

val pp_error : Format.formatter -> error -> unit
val string_of_error : error -> string

(** [tokenize src] is the tokens of [src] in order, each paired with the position
    it starts at, ending with [EOF].  A position is read as a character offset. *)
val tokenize : string -> ((Frontend.Common.token * Lexing.position) list, error) result

(** Parse one expression, allowing only whitespace and comments after it.
    Operators are taken from [operators], [Operators.default] by default.

    [budget] bounds how many parser frames may be alive at once, and so the
    nesting the parser accepts; it is spent one unit per nested call and is
    [100_000] by default.  Going over it, or overflowing the stack, is reported
    as an error rather than crashing. *)
val parse_expr
  :  ?ops:Operators.t
  -> ?max_budget:int
  -> string
  -> (Frontend.Common.expr, error) result
