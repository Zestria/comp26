[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Оба парсера принимают одно и то же ядро подмножества и строят то же дерево,
   что настоящий OCaml (ADR 0001). Всё здесь гоняется через оба. *)

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

let parsers =
  [ "menhir", (module Menhir_frontend : Parser_intf.S)
  ; "rd", (module Rd_parser : Parser_intf.S)
  ]
;;

let extensions = Config.default_extensions

let for_each_parser check =
  List.iter
    (fun (name, (module P : Parser_intf.S)) -> check name (module P : Parser_intf.S))
    parsers
;;

let parsed name parse source =
  match parse source with
  | Ok tree -> tree
  | Error message -> fail "%s: %s\n%s" name message source
;;

(* Эталонные программы из ТЗ разбираются в ожидаемое дерево. *)
let () =
  for_each_parser (fun name (module P) ->
    let structure = parsed name (P.parse_structure extensions) in
    if structure Samples.fack_source <> [ Samples.fack ]
    then fail "%s: fack разобран не в то дерево" name;
    if structure Samples.fibk_source <> [ Samples.fibk ]
    then fail "%s: fibk разобран не в то дерево" name;
    let program = String.concat "\n" [ Samples.fack_source; Samples.fibk_source ] in
    if structure program <> [ Samples.fack; Samples.fibk ]
    then fail "%s: fack и fibk вместе разобраны не в то дерево" name)
;;

(* Вывод принтера разбирается обратно в то же дерево. Применение [true] и
   [()] печатается со скобками: без них это вне ядра. *)
let () =
  let constructor_application =
    Structure_value
      ( Non_recursive
      , Samples.binding
          "x"
          (Samples.app
             (Expr_app (Expr_constant (Const_bool true), Samples.int 1))
             [ Expr_app (Expr_constant Const_unit, Samples.int 2) ])
      , [] )
  in
  let structure = [ Samples.fack; Samples.fibk; Samples.main; constructor_application ] in
  let printed = Pprint.string_of_structure structure in
  for_each_parser (fun name (module P) ->
    if parsed name (P.parse_structure extensions) printed <> structure
    then fail "%s: вывод принтера разобран не в то дерево:\n%s" name printed)
;;

(* Разбор совпадает с настоящим OCaml: приоритеты, ассоциативность, кортежи,
   унарный минус и сахар [let f x = ...]. *)
let same_as_ocaml name parse reparse source =
  match reparse source with
  | Error message -> fail "оракул не разобрал:\n%s\n%s" source message
  | Ok expected ->
    if parsed name parse source <> expected
    then fail "%s: не как в OCaml:\n%s" name source
;;

let expressions =
  [ "1 + 2 * 3"
  ; "1 - 2 - 3"
  ; "a ** b ** c"
  ; "a || b && c"
  ; "a && b || c"
  ; "a || b || c"
  ; "a = b = c"
  ; "a < b = (c <= d)"
  ; "a <> b"
  ; "a lsl b lsl c"
  ; "a lsl b ** c"
  ; "a mod b * c"
  ; "a land b lor c lxor d"
  ; "a +. b *. c -. d /. e"
  ; "f x y z"
  ; "f (g x) y"
  ; "f x + g y"
  ; "- 1"
  ; "-1"
  ; "-(1)"
  ; "-(-1)"
  ; "- - 1"
  ; "- x"
  ; "- - x"
  ; "- f x"
  ; "-1 x"
  ; "-x + 1"
  ; "- x ** 2"
  ; "-1 ** 2"
  ; "- x * y"
  ; "1 - -1"
  ; "f (-1)"
  ; "f - 1"
  ; "(- 1 + 2)"
  ; "1, 2, 3"
  ; "(1, 2), 3"
  ; "1, (2, 3)"
  ; "1 + 2, 3"
  ; "fun x -> x, x"
  ; "(fun x -> x, x)"
  ; "(fun x -> x), x"
  ; "1, fun x -> x, x"
  ; "fun x y (a, b) _ () -> a"
  ; "fun (-1) -> 0"
  ; "fun x -> fun y -> x + y"
  ; "if a then b else c + d"
  ; "if a then b else c, d"
  ; "x, if a then b else c, d"
  ; "(if a then b else c), d"
  ; "if a, b then c, d else e"
  ; "if a then if b then c else d else e"
  ; "if a then b else fun x -> x, x"
  ; "if a then let x = 1 in x else 2"
  ; "(if a then b else c) + 1"
  ; "(let x = 1 in x) + 2"
  ; "let x = 1 in let y = 2 in x, y"
  ; "(let x = 1 in x, x)"
  ; "let rec f x = g x and g y = f y in f"
  ; "let f (a, b) c = a in f"
  ; "let a, b = 1, 2 in a"
  ; "let (a, b), c = (1, 2), 3 in a"
  ; "let (+) a b = a - b in 1 + 2"
  ; "let ( * ) = ( + ) in ( * ) 1 2"
  ; "let ( = ) x = x in ( = ) 1"
  ; "let _ = () in true"
  ; "let (-1) = -1 in 0"
  ; "( mod ) 5 3"
  ; "( - ) 1"
  ; "(( + ))"
  ; "(true) x"
  ; "f true () false"
  ; "1 x"
  ; "4611686018427387903"
  ; "-4611686018427387904"
  ; "- 4611686018427387904"
  ; "x' + _y + x1"
  ; "f (* комментарий (* вложенный *) *) x"
  ]
;;

let structures =
  [ ""
  ; "let x = 1"
  ; "let x = 1 let y = 2"
  ; "let rec f n = if n <= 1 then 1 else n * f (n - 1)\nlet main = f 5"
  ; "let f = fun x -> x let g = 1"
  ; "let a = 1 and b = 2"
  ; "let (x, y), z = (1, 2), 3"
  ; "let (-4611686018427387904) = 0"
  ]
;;

let () =
  for_each_parser (fun name (module P) ->
    List.iter
      (same_as_ocaml name (P.parse_expr extensions) Reparse.expression)
      expressions;
    List.iter
      (same_as_ocaml name (P.parse_structure extensions) Reparse.structure)
      structures)
;;

(* Известное отличие от OCaml: он отвергает [-(-4611686018427387904)], а у нас
   отрицание [min_int] переполняется в него же. *)
let () =
  let source = "- (-4611686018427387904)" in
  for_each_parser (fun name (module P) ->
    if parsed name (P.parse_expr extensions) source <> Expr_constant (Const_int min_int)
    then fail "%s: не то дерево: %s" name source)
;;

(* Вне ядра: оба парсера отвечают [Error] с позицией, а не исключением и не
   деревом. Лексические ошибки у парсеров общие, поэтому и текст одинаковый;
   синтаксические --- у каждого свои. *)
let expect_error name parse source expected =
  match parse source with
  | Ok _ -> fail "%s: ожидалась ошибка:\n%s" name source
  | Error message when String.equal message expected -> ()
  | Error message ->
    fail "%s: не та ошибка на %S:\nожидалось %s\nполучено %s" name source expected message
;;

let () =
  for_each_parser (fun name (module P) ->
    let expr = expect_error name (P.parse_expr extensions) in
    let structure = expect_error name (P.parse_structure extensions) in
    expr "match x with _ -> 1" "1:1: ключевое слово match вне подмножества";
    expr "let match = 1 in match" "1:5: ключевое слово match вне подмножества";
    expr "begin 1 end" "1:1: ключевое слово begin вне подмножества";
    expr "a or b" "1:3: ключевое слово or вне подмножества";
    expr "Some 1" "1:1: конструктор или модуль Some вне подмножества";
    expr "f List.map" "1:3: конструктор или модуль List вне подмножества";
    expr "1.5" "1:1: литерал 1.5 вне подмножества: только целые";
    expr "1L" "1:1: литерал 1L вне подмножества: только целые";
    expr "0x1F" "1:1: литерал 0x1F вне подмножества: только целые";
    expr "1_000" "1:1: литерал 1_000 вне подмножества: только целые";
    expr "f 1x" "1:3: литерал 1x вне подмножества: только целые";
    expr "\"s\"" "1:1: неожиданный символ '\"'";
    expr "a ## b" "1:3: неожиданный символ '#'";
    expr "a; b" "1:2: неожиданный символ ';'";
    expr "a @ b" "1:3: оператор @ вне подмножества";
    expr "a |> f" "1:3: оператор |> вне подмножества";
    expr "a := b" "1:3: оператор := вне подмножества";
    expr "a != b" "1:3: оператор != вне подмножества";
    expr "a & b" "1:3: оператор & вне подмножества";
    expr "!x" "1:1: оператор ! вне подмножества";
    expr "~- x" "1:1: оператор ~- вне подмножества";
    expr "1+-1" "1:2: оператор +- вне подмножества";
    expr "a | b" "1:3: оператор | вне подмножества";
    expr "99999999999999999999" "1:1: целое 99999999999999999999 не помещается в int";
    expr "4611686018427387904" "1:1: целое 4611686018427387904 не помещается в int";
    expr "-(4611686018427387904)" "1:3: целое 4611686018427387904 не помещается в int";
    structure
      "let (4611686018427387904) = 1"
      "1:6: целое 4611686018427387904 не помещается в int";
    expr "x (* не закрыт\n(* *)" "1:3: незакрытый комментарий";
    expr "1 +" "1:4: неожиданный конец ввода";
    expr "let x = 1 in" "1:13: неожиданный конец ввода";
    expr "if a then b" "1:12: неожиданный конец ввода";
    expr "(1, 2" "1:6: неожиданный конец ввода";
    expr "( mod" "1:6: неожиданный конец ввода";
    structure "let" "1:4: неожиданный конец ввода";
    structure "let x =\n" "2:1: неожиданный конец ввода")
;;

(* Синтаксис вне ядра: достаточно, что это ошибка с позицией. *)
let expect_some_error name parse source =
  match parse source with
  | Ok _ -> fail "%s: ожидалась ошибка:\n%s" name source
  | Error message ->
    (match String.split_on_char ':' message with
     | line :: column :: _ :: _
       when Option.is_some (int_of_string_opt line)
            && Option.is_some (int_of_string_opt column) -> ()
     | _ -> fail "%s: ошибка без позиции на %S: %s" name source message)
;;

let () =
  for_each_parser (fun name (module P) ->
    List.iter
      (expect_some_error name (P.parse_expr extensions))
      [ "1 + if a then b else c"
      ; "1 + let x = 1 in x"
      ; "1 + fun x -> x"
      ; "- if a then b else c"
      ; "f fun x -> x"
      ; "+1"
      ; "true x"
      ; "() x"
      ; "- false x"
      ; "-. x"
      ; "fun -1 -> 0"
      ; "fun (-1, 2) -> 0"
      ; "fun x, y -> 0"
      ; "fun -> 0"
      ; "let (x) y = 1 in x"
      ; "let _ x = 1 in x"
      ; "let (a, b) x = 1 in x"
      ; "if a then b"
      ; "f ) x"
      ; "let x = 1\nin in x"
      ; "(1, 2"
      ; "( + 1 )"
      ; "x -> y"
      ];
    List.iter
      (expect_some_error name (P.parse_structure extensions))
      [ "let x = 1;; let y = 2"
      ; ";;"
      ; "let -1 = -1"
      ; "let x = 1 in x"
      ; "1 + 2"
      ; "let x = 1 let"
      ])
;;
