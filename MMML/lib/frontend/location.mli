[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Source locations used by frontend diagnostics and tokens. *)

type point =
  { offset : int
  ; line : int
  ; column : int
  }
[@@deriving show { with_path = false }]

type span =
  { start : point
  ; finish : point
  }
[@@deriving show { with_path = false }]

val of_lexing_position : Lexing.position -> point
val of_lexbuf : Lexing.lexbuf -> span
val pp_point : Format.formatter -> point -> unit
