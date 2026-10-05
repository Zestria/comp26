[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** A-нормальная форма после lambda lifting. Аргументы действий всегда атомы,
    функции находятся только на верхнем уровне. *)
type immexpr =
  | ImmNum of int
  | ImmBool of bool
  | ImmUnit
  | ImmId of Ast.identifier

type cexpr =
  | CImmExpr of immexpr
  | CApp of immexpr * immexpr list
  | CIf of cond * aexpr * aexpr
  | CTuple of immexpr * immexpr * immexpr list
  | CField of immexpr * int (** Индекс элемента кортежа, начиная с нуля. *)

and cond =
  | CondImm of immexpr
  | CondCmp of Ast.identifier * immexpr * immexpr

and aexpr =
  | ALet of Ast.identifier * cexpr * aexpr
  | ACExpr of cexpr

(** Имя, параметры и тело каждого объявления. Пустые параметры означают
    значение; флаг рекурсии относится ко всей группе. *)
type item = Ast.rec_flag * (Ast.identifier * Ast.identifier list * aexpr) list

type program = item list

(** Поднять атом или переменную до выражения. *)
val aimm : immexpr -> aexpr

val avar : Ast.identifier -> aexpr

(** Печать исполняемого OCaml. [CField] печатается через [Obj.field]: размер
    кортежа неизвестен из типа ANF. Предусловие: поле обращается к кортежу,
    индекс корректен. Это отладочная печать, не способ генерации машинного кода. *)
val pp_expression : Format.formatter -> aexpr -> unit

val pp_program : Format.formatter -> program -> unit
val string_of_expression : aexpr -> string
val string_of_program : program -> string

(** Число локальных [ALet], включая обе ветки каждого [CIf]. *)
val count_lets : program -> int
