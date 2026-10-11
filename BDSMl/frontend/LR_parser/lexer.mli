[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error = [ `lexer_error of string ]

exception Error of string

(** Read one token using ocamllex. Raises [Error] on invalid input. *)
val token : Lexing.lexbuf -> Tokens.token

(** Lex tokens on demand from a shared lexing buffer, preserving positions for Menhir. *)
val tokenize_lazy_from_lexbuf
  :  Lexing.lexbuf
  -> (Tokens.token, [> error ]) result Seq.t

(** Lex tokens on demand, ending with [TEOF] or a lexer error. *)
val tokenize_lazy : string -> (Tokens.token, [> error ]) result Seq.t

(** Collect the same tokens as [tokenize_lazy] into a list. *)
val tokenize : string -> (Tokens.token list, [> error ]) result
