[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let reserved =
  [ "as"
  ; "assert"
  ; "begin"
  ; "class"
  ; "constraint"
  ; "do"
  ; "done"
  ; "downto"
  ; "effect"
  ; "end"
  ; "exception"
  ; "external"
  ; "for"
  ; "function"
  ; "functor"
  ; "include"
  ; "inherit"
  ; "initializer"
  ; "lazy"
  ; "match"
  ; "method"
  ; "module"
  ; "mutable"
  ; "new"
  ; "nonrec"
  ; "object"
  ; "of"
  ; "open"
  ; "or"
  ; "private"
  ; "sig"
  ; "struct"
  ; "to"
  ; "try"
  ; "type"
  ; "val"
  ; "virtual"
  ; "when"
  ; "while"
  ; "with"
  ]
;;

type associativity =
  | Left
  | Right

let integer_additive = [ "+"; "-" ]
let integer_multiplicative = [ "*"; "/"; "mod"; "land"; "lor"; "lxor" ]
let integer_shifts = [ "lsl"; "lsr"; "asr" ]

let integer_operators =
  integer_additive @ integer_multiplicative @ integer_shifts @ [ "~-" ]
;;

let binary_operators =
  [ [ "||" ], Right
  ; [ "&&" ], Right
  ; [ "="; "<>"; "<"; "<="; ">"; ">=" ], Left
  ; integer_additive @ [ "+."; "-." ], Left
  ; integer_multiplicative @ [ "*."; "/." ], Left
  ; "**" :: integer_shifts, Right
  ]
;;

let operators = List.concat_map fst binary_operators
let builtins = operators @ [ "~-" ] @ [ "print_int" ]

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let reserved_error word = Printf.sprintf "ключевое слово %s вне подмножества" word

let constructor_error name =
  Printf.sprintf "конструктор или модуль %s вне подмножества" name
;;

let operator_error name = Printf.sprintf "оператор %s вне подмножества" name

let literal_error literal =
  Printf.sprintf "литерал %s вне подмножества: только целые" literal
;;

let int_overflow_error digits = Printf.sprintf "целое %s не помещается в int" digits

let unexpected_char_error char = Printf.sprintf "неожиданный символ %C" char

let unclosed_comment_error = "незакрытый комментарий"
let end_of_input_error = "неожиданный конец ввода"
