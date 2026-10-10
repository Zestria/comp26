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

type associativity =
  | Left
  | Right

(** Бинарные операторы подмножества по уровням, от слабого к сильному
    (ADR 0001). Нисходящий парсер держит свою копию: в №5 её переопределяют на
    лету. *)
val binary_operators : (string list * associativity) list

(** Все бинарные операторы подмножества, в том числе словесные ([mod], [lsl], …). *)
val operators : string list

(** Имена, приходящие из окружения исполнения: операторы, [~-] и [print_int].
    [~-] --- внутреннее имя унарного минуса: парсеры строят его из [-e], а
    написать [~-] в исходнике нельзя (ADR 0001).
    Rename не переименовывает их, closure conversion считает их глобальными.
    Список один на оба прохода, чтобы они не разошлись. *)
val builtins : string list

(** Встроенные операции с целочисленным результатом, включая унарный [~-].
    ANF использует их, чтобы распознать сравнение целых без вывода типов.
    Полиморфные сравнения и операции с float сюда не входят. *)
val integer_operators : string list

(** Десятичная цифра [0]–[9]: из них состоит целая константа подмножества. *)
val is_digit : char -> bool

(** Сообщения об ошибках, одинаковые в обоих парсерах. *)

val reserved_error : string -> string
val constructor_error : string -> string
val operator_error : string -> string
val literal_error : string -> string
val int_overflow_error : string -> string
val unexpected_char_error : char -> string
val unclosed_comment_error : string
val end_of_input_error : string
