[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Лексер для Menhir-парсера. *)

(** Лексическая ошибка: позиция начала проблемного места и описание. *)
exception Error of Lexing.position * string

(** Прочитать следующий токен. Бросает [Error]. *)
val token : Lexing.lexbuf -> Menhir_parser.token
