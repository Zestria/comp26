(* SPDX-License-Identifier: LGPL-3.0-or-later *)

exception Error of string

val token : Lexing.lexbuf -> Grammar.token
