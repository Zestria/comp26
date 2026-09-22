[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Эталонные программы из ТЗ и конструкторы деревьев для тестов. Общие для
    принтера и обоих парсеров, чтобы все проверялись против одного дерева. *)

open Vibecaml.Ast

(** Целая константа. *)
val int : int -> expression

(** Идентификатор. *)
val var : identifier -> expression

(** Применение к нескольким аргументам: [app f [a; b]] --- это [f a b]. *)
val app : expression -> expression list -> expression

(** Бинарный оператор: [binary "+" l r] --- это [( + ) l r]. *)
val binary : identifier -> expression -> expression -> expression

(** Связывание [name = expression]. *)
val binding : identifier -> expression -> value_binding

(** Исходник факториала в CPS, дословно из ТЗ. *)
val fack_source : string

(** Дерево [fack_source]. *)
val fack : structure_item

(** Исходник Фибоначчи в CPS, дословно из ТЗ. *)
val fibk_source : string

(** Дерево [fibk_source]. *)
val fibk : structure_item

(** [let main = (fack 5 (fun x -> x), fibk 10 (fun x -> x))]. *)
val main : structure_item
