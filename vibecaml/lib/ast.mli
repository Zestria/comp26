(** Абстрактное синтаксическое дерево подмножества OCaml.

    Операторы представляются применением идентификатора оператора, поэтому
    изменение синтаксиса операторов не меняет представление дерева. *)

type identifier = string

(** Константы, допустимые в исходной программе. *)
type constant =
  | Const_int of int
  | Const_bool of bool
  | Const_unit

(** [Recursive] соответствует [let rec], а [Non_recursive] — обычному [let]. *)
type rec_flag =
  | Non_recursive
  | Recursive

(** Паттерны, допустимые в связываниях и аргументах функций. *)
type pattern =
  | Pat_wildcard
  | Pat_var of identifier
  | Pat_constant of constant
  | Pat_tuple of pattern * pattern * pattern list

(** Выражения языка. Кортежи содержат не менее двух элементов. *)
type expression =
  | Expr_constant of constant
  | Expr_ident of identifier
  | Expr_let of rec_flag * value_binding list * expression
  | Expr_fun of pattern list * expression
  | Expr_app of expression * expression
  | Expr_if of expression * expression * expression
  | Expr_tuple of expression * expression * expression list

and value_binding =
  { binding_pattern : pattern
  ; binding_expression : expression
  }

(** Объявление значения верхнего уровня, в том числе группа [let rec ... and ...]. *)
type structure_item = Structure_value of rec_flag * value_binding list

(** Файл программы после разбора. *)
type structure = structure_item list
