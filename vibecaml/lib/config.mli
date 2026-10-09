[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Настройки компилятора из командной строки: выбор парсера, расширения
    синтаксиса, стадия остановки, отладочный вывод, входной и выходной файлы. *)

(** Какой синтаксический анализатор запускать. *)
type parser =
  | Recursive_descent (** нисходящий, [-parsing topdown] *)
  | Menhir (** восходящий, [-parsing menhir] *)

(** Стадия конвейера, после которой компилятор печатает результат. *)
type stage =
  | Parsing (** [--stop-after parsing] *)
  | Rename (** [--stop-after rename] *)
  | Closure_conversion (** [--stop-after cc] *)
  | Lambda_lifting (** [--stop-after ll] *)
  | Anf (** [--stop-after anf] *)

(** Расширения синтаксиса --- общий тип из [Parser_intf], см. #5. *)
type extensions = Parser_intf.extensions

(** Все расширения выключены. *)
val default_extensions : extensions

type t =
  { parser : parser
  ; extensions : extensions
  ; stop_after : stage option (** [None] --- весь конвейер *)
  ; dump_ast : bool (** напечатать AST, [--dump-ast] *)
  ; dump_pretty_printed : bool (** напечатать программу принтером, [--dump-pp] *)
  ; input_file : string option (** позиционный аргумент; [None] --- не задан *)
  ; output_file : string option (** [-o]; [None] или ["-"] --- stdout *)
  }

(** Нисходящий парсер, расширения выключены, весь конвейер, отладочного вывода
    нет, входного и выходного файлов нет. *)
val default : t

(** Разобрать аргументы командной строки без побочных эффектов.

    Массив устроен как [Sys.argv]: нулевой элемент --- имя программы, он
    пропускается. Аргумент, не начинающийся с [-], считается входным файлом;
    одиночный [-] тоже попадает в [input_file], что он значит --- решает
    драйвер. Ошибка --- неизвестный флаг или значение, флаг без значения,
    пустое имя файла, второй входной или выходной файл, флаг дампа вместе со
    стадией, отличной от [parsing]. *)
val parse_arguments : string array -> (t, string) result
