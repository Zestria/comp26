[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Разбор вывода принтера настоящим парсером OCaml из [compiler-libs] и перевод
    Parsetree обратно в наш AST. Нужен тестам: раз наших парсеров ещё нет,
    переразбираемость проверяем эталонным. *)

(** Разобрать выражение. Ошибка --- синтаксическая или конструкция вне
    подмножества. *)
val expression : string -> (Vibecaml.Ast.expression, string) result

(** Разобрать программу. *)
val structure : string -> (Vibecaml.Ast.structure, string) result
