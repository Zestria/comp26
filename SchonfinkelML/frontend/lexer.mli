[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Common

type error =
  { pos : Lexing.position
  ; msg : string
  }

exception Lexing_error of error

val pp_error : Format.formatter -> error -> unit
val token : Lexing.lexbuf -> token

(* The generated lexer defines this helper, but the string-literal rule is
   currently disabled in [lexer.mll], so it is unused.  It must stay in the
   interface, otherwise the build fails on unused-value warning 32. *)
val lex_string : Lexing.lexbuf -> string -> string
