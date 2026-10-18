[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Абстрактное синтаксическое дерево подмножества OCaml.

    Операторы представляются применением идентификатора оператора, поэтому
    изменение синтаксиса операторов не меняет представление дерева. *)

type identifier = string [@@deriving show { with_path = false }]

(** Константы, допустимые в исходной программе. *)
type constant =
  | Const_int of int (** [42], [-1] *)
  | Const_bool of bool (** [true], [false] *)
  | Const_unit (** [()] *)
[@@deriving show { with_path = false }]

(** [Recursive] соответствует [let rec], а [Non_recursive] --- обычному [let]. *)
type rec_flag =
  | Non_recursive (** [let] *)
  | Recursive (** [let rec] *)
[@@deriving show { with_path = false }]

(** Паттерны, допустимые в связываниях и аргументах функций. *)
type pattern =
  | Pat_wildcard (** [_] *)
  | Pat_var of identifier (** [x], [( + )] *)
  | Pat_constant of constant (** [1], [true], [()] *)
  | Pat_tuple of pattern * pattern * pattern list (** [(p1, p2, ...)] *)
[@@deriving show { with_path = false }]

(** Выражения языка.

    Непустота списков выражена типом: у кортежа не меньше двух элементов, у
    [fun] не меньше одного параметра, в группе [let ... and ...] не меньше одного
    связывания. *)
type expression =
  | Expr_constant of constant (** константа *)
  | Expr_ident of identifier (** переменная или оператор: [x], [( + )] *)
  | Expr_let of rec_flag * value_binding * value_binding list * expression
  (** [let [rec] b1 and ... in e] *)
  | Expr_fun of pattern * pattern list * expression (** [fun p1 ... pn -> e] *)
  | Expr_app of expression * expression
  (** [f x]; [f x y] --- это [Expr_app (Expr_app (f, x), y)] *)
  | Expr_if of expression * expression * expression (** [if c then t else e] *)
  | Expr_tuple of expression * expression * expression list (** [(e1, e2, ...)] *)

(** Связывание [p = e] внутри [let]. *)
and value_binding =
  { binding_pattern : pattern
  ; binding_expression : expression
  }
[@@deriving show { with_path = false }]

(** Объявление верхнего уровня. *)
type structure_item =
  | Structure_value of rec_flag * value_binding * value_binding list
  (** [let [rec] b1 and ...], в том числе группа [let rec ... and ...] *)
[@@deriving show { with_path = false }]

(** Файл программы после разбора. *)
type structure = structure_item list [@@deriving show { with_path = false }]

(** Имена, связываемые паттерном, слева направо. Дубли не убираются:
    после [Rename.rename] их нет. *)
val pattern_names : pattern -> identifier list
