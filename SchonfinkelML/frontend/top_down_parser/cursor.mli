[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Cursor over the source string.

    Char mechanics only; parsing policy lives in {!Parser}. *)

type t
(** Source, offset, line number and line start. *)

type state
(** Snapshot for backtracking. *)

type error =
  { pos : Lexing.position
  ; msg : string
  }
(** Error with a [Lexing.position]. *)

val create : string -> t
val source : t -> string
val offset : t -> int
val position : t -> Lexing.position
val error : t -> string -> error

val save : t -> state
(** Cheap snapshot. *)

val restore : t -> state -> unit
(** Roll back to a snapshot. *)

val eof : t -> bool
val peek : t -> char option
(** Current char. *)

val peek_at : t -> int -> char option
(** Char at offset [n]. *)

val advance : t -> unit
(** Consume one char, tracking lines. *)

val advance_n : t -> int -> unit
val is_digit : char -> bool
val is_ident_start : char -> bool
val is_ident_char : char -> bool
val starts_with : t -> string -> bool
(** Does the source start with [s] here? *)

val peek_ident_run : t -> string
(** Longest [ident_char] run from here, [""] if none. *)

val peek_int_run : t -> string
(** Longest digit run from here. *)

val skip_ws_and_comments : t -> (unit, error) result
(** Skip whitespace and nested comments; [Error] on an unclosed comment. *)
