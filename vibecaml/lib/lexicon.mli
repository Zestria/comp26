[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Лексика, общая для обоих парсеров: какие слова и операторы входят в
    подмножество. Держим в одном месте, чтобы парсеры не разошлись в том, что
    считать ошибкой. *)

(** Ключевые слова OCaml, которых нет в подмножестве. На них ошибка, а не
    идентификатор: наш вход всегда остаётся корректным OCaml. *)
val reserved : string list

(** Бинарные операторы подмножества, в том числе словесные ([mod], [lsl], …). *)
val operators : string list

(** Сообщения об ошибках, одинаковые в обоих парсерах. *)

val reserved_error : string -> string
val constructor_error : string -> string
val operator_error : string -> string
val literal_error : string -> string
val int_overflow_error : string -> string
val unexpected_char_error : char -> string
val unclosed_comment_error : string
val end_of_input_error : string
