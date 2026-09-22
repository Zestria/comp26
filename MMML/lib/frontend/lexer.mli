[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Demand-driven lexical analysis for MiniML. *)

(** Abstract state of a buffered token stream. *)
type t

(** Create a lexer over an existing string. Tokens are recognized only when
    {!next_token} is called. *)
val from_string : string -> t

(** Create a buffered lexer which reads [channel] on demand. Closing the
    channel remains the caller's responsibility. *)
val from_channel : in_channel -> t

(** Create a buffered lexer over standard input. *)
val from_stdin : unit -> t

(** Create a lexer backed by a refill callback. The callback is invoked only
    when the lexer needs more input. Returning zero signals end of input. *)
val from_function : (bytes -> int -> int) -> t

(** Consume and return one located token, skipping whitespace and comments.
    Lexical failures are returned without raising an exception. *)
val next_token : t -> Token.located Error_monad.t
