(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error = [ `lexer_error of string ]

exception Error of string

(** Read one token using ocamllex. Raises [Error] on invalid input. *)
val token : Lexing.lexbuf -> Lexer.token

(** Use the same token type and error variant as [Lexer.tokenize]. *)
val tokenize : string -> (Lexer.token list, [> error ]) result
