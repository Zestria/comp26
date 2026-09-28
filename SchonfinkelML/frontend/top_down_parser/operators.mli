[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** The operator table: which tokens act as unary (prefix) or binary (infix)
    operators, with which precedence and associativity.

    The parser is driven by this table instead of by hard-coded levels, so a
    table coming from the command line can redefine both the arity of an
    operator ([prefix 9 -] against [left 6 -]) and its associativity
    ([left 6 +] against [right 6 +] against [nonassoc 4 =]). *)

type assoc =
  | Left
  | Right
  | Nonassoc

type fixity =
  | Prefix
  | Infix of assoc

(** An entry binds the spelling [sym] (as it is written in a table) to the
    token [tok] the lexer produces for it, to a [fixity] and to a [prec]; only
    the relative order of precedences matters, a larger [prec] binds tighter. *)
type entry =
  { sym : string
  ; tok : Frontend.Common.token
  ; fixity : fixity
  ; prec : int
  }

type t = entry list

val token_of_sym : string -> Frontend.Common.token option

(** The table used when no table is given on the command line, that is
    ["left 4 = < >; left 6 + -; left 7 * /; prefix 9 -"]. *)
val default : t

(** [of_string s] parses a table written as groups separated by [;], each group
    being a fixity keyword ([left], [right], [nonassoc] or [prefix]), a
    precedence number and one or more operator spellings, for instance
    ["left 6 + -; right 7 *; prefix 9 -; nonassoc 4 ="].  The result replaces
    the whole table, and the same spelling may not be defined twice with the
    same fixity, although it may appear both as an infix and as a prefix
    operator. *)
val of_string : string -> (t, string) result

(** [pp ppf t] prints [t] in the syntax accepted by [of_string]; entries that
    are adjacent and agree on fixity and precedence are printed as one group. *)
val pp : Format.formatter -> t -> unit

val string_of : t -> string

(** [find_infix t tok] is the entry of [tok], provided [t] makes it a binary
    operator. *)
val find_infix : t -> Frontend.Common.token -> entry option

(** [find_prefix t tok] is the entry of [tok], provided [t] makes it a prefix
    (unary) operator.  A token may be both an infix and a prefix operator, as
    [-] is in [default]. *)
val find_prefix : t -> Frontend.Common.token -> entry option
