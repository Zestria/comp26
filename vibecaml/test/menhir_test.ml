[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml
open Ast

(* Не [failwith]: необработанное исключение печатает кириллицу экранированной. *)
let fail format =
  Printf.ksprintf
    (fun message ->
       prerr_endline message;
       exit 1)
    format
;;

let parse_structure source =
  match Menhir_frontend.parse_structure source with
  | Ok structure -> structure
  | Error message -> fail "%s\n%s" message source
;;

let parse_expr source =
  match Menhir_frontend.parse_expr source with
  | Ok expression -> expression
  | Error message -> fail "%s\n%s" message source
;;

(* Эталонные программы из ТЗ разбираются в ожидаемое дерево. *)
let () =
  if parse_structure Samples.fack_source <> [ Samples.fack ]
  then fail "fack разобран не в то дерево";
  if parse_structure Samples.fibk_source <> [ Samples.fibk ]
  then fail "fibk разобран не в то дерево";
  let program = String.concat "\n" [ Samples.fack_source; Samples.fibk_source ] in
  if parse_structure program <> [ Samples.fack; Samples.fibk ]
  then fail "fack и fibk вместе разобраны не в то дерево"
;;

(* Вывод принтера разбирается обратно в то же дерево. *)
let () =
  let structure = [ Samples.fack; Samples.fibk; Samples.main ] in
  let printed = Pprint.string_of_structure structure in
  if parse_structure printed <> structure
  then fail "вывод принтера разобран не в то дерево:\n%s" printed
;;

(* Разбор совпадает с настоящим OCaml: приоритеты, ассоциативность, кортежи,
   унарный минус и сахар [let f x = ...]. *)
let same_as_ocaml parse reparse source =
  match reparse source with
  | Error message -> fail "оракул не разобрал:\n%s\n%s" source message
  | Ok expected -> if parse source <> expected then fail "не как в OCaml:\n%s" source
;;

let () =
  List.iter
    (same_as_ocaml parse_expr Reparse.expression)
    [ "1 + 2 * 3"
    ; "1 - 2 - 3"
    ; "a @ b @ c"
    ; "a ** b ** c"
    ; "a || b && c"
    ; "a && b || c"
    ; "a || b || c"
    ; "a or b & c"
    ; "a = b = c"
    ; "a < b = (c <= d)"
    ; "a := b := c"
    ; "a |> f |> g"
    ; "f @@ g @@ x"
    ; "a lsl b lsl c"
    ; "a mod b * c"
    ; "a +. b *. c -. d"
    ; "a != b"
    ; "f x y z"
    ; "f (g x) y"
    ; "f x + g y"
    ; "- 1"
    ; "-1"
    ; "-(1)"
    ; "-(-1)"
    ; "- x"
    ; "- f x"
    ; "-x + 1"
    ; "- x ** 2"
    ; "f (-1)"
    ; "f - 1"
    ; "+1"
    ; "+x"
    ; "-. x"
    ; "~- x"
    ; "~-1"
    ; "!x"
    ; "!x y"
    ; "f x ## g y"
    ; "!x ## y"
    ; "1, 2, 3"
    ; "(1, 2), 3"
    ; "1 + 2, 3"
    ; "a := 1, 2"
    ; "fun x -> x, x"
    ; "fun x y (a, b) _ () -> a"
    ; "fun (-1) -> 0"
    ; "fun x -> fun y -> x + y"
    ; "if a then b else c + d"
    ; "if a then b else c, d"
    ; "if a then if b then c else d else e"
    ; "1 + if a then b else c"
    ; "1 + let x = 1 in x + 2"
    ; "let x = 1 in let y = 2 in x, y"
    ; "let rec f x = g x and g y = f y in f"
    ; "let f (a, b) c = a in f"
    ; "let a, b = 1, 2 in a"
    ; "let (+) a b = a - b in 1 + 2"
    ; "let ( * ) = ( + ) in ( * ) 1 2"
    ; "let _ = () in true"
    ; "( mod ) 5 3"
    ; "( ~- ) 1"
    ; "( ! ) x"
    ; "(( + ))"
    ; "begin 1 + 2 end * 3"
    ; "begin end"
    ; "true x"
    ; "1_000 + 0x1F + 0o17 + 0b101"
    ; "4611686018427387903"
    ; "-4611686018427387904"
    ; "-(4611686018427387904)"
    ; "- begin (4611686018427387904) end"
    ; "-0x4000000000000000"
    ; "- (0x4000000000000000)"
    ; "0x7FFFFFFFFFFFFFFF"
    ; "x' + _y + x1"
    ; "f (* комментарий (* вложенный *) *) x"
    ]
;;

let () =
  List.iter
    (same_as_ocaml parse_structure Reparse.structure)
    [ ""
    ; "let x = 1"
    ; "let x = 1;; let y = 2"
    ; ";; let x = 1 ;; ;;"
    ; "let rec f n = if n <= 1 then 1 else n * f (n - 1)\nlet main = f 5"
    ; "let f = fun x -> x let g = 1"
    ; "let a = 1 and b = 2"
    ; "let (x, y), z = (1, 2), 3"
    ; "let -1 = -1"
    ]
;;

(* Известное отличие от OCaml: он отвергает [-(-4611686018427387904)], а у нас
   отрицание [min_int] переполняется в него же. *)
let () =
  let source = "- (-4611686018427387904)" in
  if parse_expr source <> Expr_constant (Const_int min_int)
  then fail "не то дерево: %s" source
;;

(* Ошибочный вход даёт [Error] с позицией, а не исключение. *)
let expect_error parse source expected =
  match parse source with
  | Ok _ -> fail "ожидалась ошибка:\n%s" source
  | Error message when String.equal message expected -> ()
  | Error message ->
    fail "не та ошибка на %S:\nожидалось %s\nполучено %s" source expected message
;;

let () =
  let structure = Menhir_frontend.parse_structure in
  let expr = Menhir_frontend.parse_expr in
  expect_error expr "1 +" "1:4: неожиданный конец ввода";
  expect_error expr "let x = 1 in" "1:13: неожиданный конец ввода";
  expect_error expr "if a then b" "1:12: неожиданный конец ввода";
  expect_error expr "(1, 2" "1:6: неожиданный конец ввода";
  expect_error expr "f ) x" "1:3: синтаксическая ошибка у \")\"";
  expect_error expr "let x = 1\nin in x" "2:4: синтаксическая ошибка у \"in\"";
  expect_error expr "match x with _ -> 1" "1:1: ключевое слово match вне подмножества";
  expect_error expr "Some 1" "1:1: конструктор или модуль Some вне подмножества";
  expect_error expr "1.5" "1:1: вещественное число 1.5 вне подмножества";
  expect_error expr "1L" "1:1: литерал 1L вне подмножества: только int";
  expect_error expr "\"s\"" "1:1: неожиданный символ '\"'";
  expect_error expr "a | b" "1:3: символ | вне подмножества";
  expect_error
    expr
    "4611686018427387904"
    "1:1: синтаксическая ошибка у \"4611686018427387904\"";
  expect_error
    expr
    "99999999999999999999"
    "1:1: целое 99999999999999999999 не помещается в int";
  expect_error expr "x (* не закрыт\n(* *)" "1:3: незакрытый комментарий";
  expect_error structure "let x = 1 in x" "1:11: синтаксическая ошибка у \"in\"";
  expect_error structure "1 + 2" "1:1: синтаксическая ошибка у \"1\"";
  expect_error structure "let" "1:4: неожиданный конец ввода"
;;
