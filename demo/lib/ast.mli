(** Абстрактное синтаксическое дерево первого этапа компилятора.

    В дереве есть только конструкции, нужные для небольших функциональных
    программ. Типы и позиции в исходном тексте будут добавлены позднее. *)

(** Имена переменных из исходной программы. *)
type identifier = string

(** [Recursive] соответствует [let rec], а [Non_recursive] — обычному [let]. *)
type rec_flag =
  | Non_recursive
  | Recursive

(** Литералы, поддерживаемые языком. *)
type constant =
  | Integer of int
  | Boolean of bool

(** Шаблоны, допустимые в аргументах функций и в [let]-связываниях. *)
type pattern =
  | Variable of identifier
  | Wildcard

(** Префиксные операторы. *)
type unary_operator =
  | Negate
  | Not

(** Инфиксные операторы вынесены из выражений, чтобы оба парсера строили
    одинаковое представление. *)
type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Equal
  | Not_equal
  | Less
  | Less_or_equal
  | Greater
  | Greater_or_equal
  | And
  | Or

(** Выражения языка.

    Функция может принимать несколько аргументов. Это повторяет синтаксис
    OCaml; позднее такую функцию можно преобразовать в цепочку одноаргументных
    функций. *)
type expression =
  | Constant of constant
  | Identifier of identifier
  | Unary_operation of unary_operator * expression
  | Binary_operation of binary_operator * expression * expression
  | If_then_else of expression * expression * expression
  | Let of rec_flag * pattern * expression * expression
  | Function of pattern list * expression
  | Application of expression * expression list
