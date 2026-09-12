[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Нисходящий парсер подмножества OCaml: рекурсивный спуск без отдельного
    лексера (scannerless).

    Парсер сам решает, где кончается лексема, поэтому умеет то, что не
    выражается парой ocamllex+menhir: ключевое слово можно писать слитно с
    идентификатором. Режим включается флагом [glued]:

    - [glued:false] — обычный OCaml-подобный лексический режим: ключевое слово
      должно заканчиваться на границе ([letx] — это один идентификатор);
    - [glued:true] — ключевое слово может быть префиксом
      ([letx=1inx] — это [let x = 1 in x], [funx->x] — [fun x -> x]).

    Разбор всегда возвращает [result] и никогда не завершает процесс: на любом
    входе — [Ok] или [Error] с позицией.

    Вне области видимости (осознанно): [()] и unit, [true]/[false], float и
    string, [match], toplevel-определения, [and], []. *)

type error = Cursor.error =
  { pos : Lexing.position
  ; msg : string
  }

val pp_error : Format.formatter -> error -> unit
val string_of_error : error -> string
val parse_expr : ?glued:bool -> string -> (Frontend.Common.expr, error) result
(** Разбирает одно выражение и требует, чтобы после него остались только
    пробелы и комментарии. @param glued режим слипшихся ключевых слов (по
    умолчанию [false]). *)
