[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Cursor over the source string.

    Char mechanics only; parsing policy lives in {!Parser}. *)

(** Source, offset, line number and line start. *)
type t

(** Snapshot for backtracking. *)
type state

(** Error with a [Lexing.position]. *)
type error =
  { pos : Lexing.position
  ; msg : string
  }

val create : string -> t
val source : t -> string
val offset : t -> int
val position : t -> Lexing.position
val error : t -> string -> error

(** Cheap snapshot. *)
val save : t -> state

(** Roll back to a snapshot. *)
val restore : t -> state -> unit

val eof : t -> bool

(** Current char. *)
val peek : t -> char option

(** Char at offset [n]. *)
val peek_at : t -> int -> char option

(** Consume one char, tracking lines. *)
val advance : t -> unit

val advance_n : t -> int -> unit
val is_digit : char -> bool
val is_ident_start : char -> bool
val is_ident_char : char -> bool

(** Does the source start with [s] here? *)
val starts_with : t -> string -> bool

(** Longest [ident_char] run from here, [""] if none. *)
val peek_ident_run : t -> string

(** Longest digit run from here. *)
val peek_int_run : t -> string

(** Skip whitespace and nested comments; [Error] on an unclosed comment. *)
val skip_ws_and_comments : t -> (unit, error) result
