[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Случайные деревья для QuickCheck и их сжатие. Общие для принтера и обоих
    парсеров.

    Генератор покрывает ровно то, что разбирают парсеры: расширяя парсер,
    расширяй и генератор, иначе свойство зелёное и ничего не проверяет. *)

open Vibecaml.Ast

(** Имена ядра (ADR 0001): их принимают оба наших парсера. *)
val core_identifiers : identifier list

(** Ядро плюс операторы, которые принимает только OCaml. Для проверки
    принтера оракулом [Reparse]. *)
val wide_identifiers : identifier list

(** Выражение с именами из списка. *)
val expression : identifier list -> expression QCheck.Gen.t

(** Программа с именами из списка. *)
val structure : identifier list -> structure QCheck.Gen.t

(** Сжатие до поддеревьев, простых листьев и коротких списков. Не выводит за
    ядро: новые имена не появляются, кроме [x]. *)
val shrink_expression : expression QCheck.Shrink.t

val shrink_structure : structure QCheck.Shrink.t

(** Прогнать свойства. Seed случайный, чтобы каждый прогон проверял новые
    деревья; при падении он печатается, и прогон повторяется через
    [QCHECK_SEED=<seed>]. Падение завершает процесс с кодом 1. *)
val check : QCheck.Test.t list -> unit
