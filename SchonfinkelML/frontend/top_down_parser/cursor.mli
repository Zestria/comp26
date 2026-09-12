[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Курсор по исходной строке.

    Модуль знает только «механику»: позиции, символы, пробелы, комментарии и
    прогоны символов. Политику разбора (в том числе режим слипшихся ключевых
    слов) определяет {!Parser}. *)

type t
(** Состояние: строка, смещение, номер строки и начало текущей строки. *)

type state
(** Снимок состояния для отката (backtracking). *)

type error =
  { pos : Lexing.position
  ; msg : string
  }
(** Ошибка с позицией. Позиции совместимы с [Lexing.position], чтобы сообщения
    выглядели как у обычного лексера. *)

val create : string -> t
val source : t -> string
val offset : t -> int
val position : t -> Lexing.position
val error : t -> string -> error

val save : t -> state
(** Снимок текущего состояния. Снимок дешёвый: три целых. *)

val restore : t -> state -> unit
(** Возврат к снимку. *)

val eof : t -> bool
val peek : t -> char option
(** Символ в текущей позиции. *)

val peek_at : t -> int -> char option
(** [peek_at t n] — символ со сдвигом [n] от текущей позиции. *)

val advance : t -> unit
(** Сдвиг на один символ; перевод строки обновляет [pos_lnum]/[pos_bol]. *)

val advance_n : t -> int -> unit
val is_digit : char -> bool
val is_ident_start : char -> bool
val is_ident_char : char -> bool
val starts_with : t -> string -> bool
(** Начинается ли источник с [s] в текущей позиции. *)

val peek_ident_run : t -> string
(** Максимальный прогон [ident_char] от текущей позиции; [""], если символа
    нет или он не может начинать идентификатор. *)

val peek_int_run : t -> string
(** Максимальный прогон цифр от текущей позиции. *)

val skip_ws_and_comments : t -> (unit, error) result
(** Пропускает пробелы и вложенные комментарии [(* ... *)]. Ошибка —
    незакрытый комментарий. *)
