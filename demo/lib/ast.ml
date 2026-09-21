(** Абстрактное синтаксическое дерево первого этапа компилятора.

    Реализация намеренно повторяет интерфейс: парсерам и следующим проходам
    компилятора нужен прозрачный доступ к представлению дерева. *)

type identifier = string

type rec_flag =
  | Non_recursive
  | Recursive

type constant =
  | Integer of int
  | Boolean of bool

type pattern =
  | Variable of identifier
  | Wildcard

type unary_operator =
  | Negate
  | Not

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

type expression =
  | Constant of constant
  | Identifier of identifier
  | Unary_operation of unary_operator * expression
  | Binary_operation of binary_operator * expression * expression
  | If_then_else of expression * expression * expression
  | Let of rec_flag * pattern * expression * expression
  | Function of pattern list * expression
  | Application of expression * expression list
