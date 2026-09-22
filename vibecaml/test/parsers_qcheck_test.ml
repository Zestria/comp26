[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Свойство [parse (pp ast) = ast] для обоих парсеров на одних и тех же
   деревьях: расхождение парсеров всплывает сразу. *)

open Vibecaml
open Ast
open Samples

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

let round_trip print parse tree =
  let printed = print tree in
  match parse printed with
  | Ok parsed when parsed = tree -> true
  | Ok parsed -> QCheck.Test.fail_reportf "%s\nразобрано как\n%s" printed (print parsed)
  | Error message -> QCheck.Test.fail_reportf "%s\n%s" message printed
;;

let () =
  let identifiers = Generate.core_identifiers in
  Generate.check
    (List.concat_map
       (fun (name, (module P : Parser_intf.S)) ->
          [ QCheck.Test.make
              ~count:1000
              ~name:(name ^ ": выражение разбирается в то же дерево")
              (QCheck.make
                 ~print:Pprint.string_of_expression
                 ~shrink:Generate.shrink_expression
                 (Generate.expression identifiers))
              (round_trip Pprint.string_of_expression P.parse_expr)
          ; QCheck.Test.make
              ~count:1000
              ~name:(name ^ ": программа разбирается в то же дерево")
              (QCheck.make
                 ~print:Pprint.string_of_structure
                 ~shrink:Generate.shrink_structure
                 (Generate.structure identifiers))
              (round_trip Pprint.string_of_structure P.parse_structure)
          ])
       parsers)
;;

(* Проверки самого генератора: свойство зелёное и тогда, когда генератор не
   выдаёт нужную форму, или когда shrink выдаёт дерево, которого парсер не
   строит, --- тогда контрпример ложный. *)

let rec exists predicate expression =
  predicate expression
  ||
  match expression with
  | Expr_constant _ | Expr_ident _ -> false
  | Expr_let (_, first, rest, body) ->
    List.exists
      (fun binding -> exists predicate binding.binding_expression)
      (first :: rest)
    || exists predicate body
  | Expr_fun (_, _, body) -> exists predicate body
  | Expr_app (function_, argument) ->
    exists predicate function_ || exists predicate argument
  | Expr_if (condition, then_branch, else_branch) ->
    List.exists (exists predicate) [ condition; then_branch; else_branch ]
  | Expr_tuple (first, second, rest) ->
    List.exists (exists predicate) (first :: second :: rest)
;;

(* Сжатие не выдаёт [( ~- ) 1] и голый [( ~- )]: минус здесь стоит перед тем,
   что сжимается в целую константу. Кандидаты проверяются на два шага. *)
let () =
  let neg operand = app (var "~-") [ operand ] in
  let check_candidate origin candidate =
    let printed = Pprint.string_of_expression candidate in
    if candidate = origin then fail "shrink: кандидат равен исходному: %s" printed;
    List.iter
      (fun (name, (module P : Parser_intf.S)) ->
         match P.parse_expr printed with
         | Ok parsed when parsed = candidate -> ()
         | Ok _ | Error _ ->
           fail
             "shrink вывел за ядро (%s): %s\nиз %s"
             name
             printed
             (Pprint.string_of_expression origin))
      parsers
  in
  List.iter
    (fun tree ->
       Generate.shrink_expression tree (fun candidate ->
         check_candidate tree candidate;
         Generate.shrink_expression candidate (check_candidate candidate)))
    [ neg (app (int 5) [ var "x" ])
    ; neg (Expr_constant (Const_bool true))
    ; neg (Expr_constant Const_unit)
    ; neg (Expr_if (int 1, int 2, int 3))
    ; neg (Expr_let (Non_recursive, binding "x" (int 1), [], int 2))
    ; neg (Expr_tuple (int 1, int 2, []))
    ; neg (Expr_fun (Pat_var "x", [], int 1))
    ; neg (neg (app (int min_int) [ Expr_constant (Const_bool false) ]))
    ; app (neg (app (int 1) [ var "x" ])) [ int 2 ]
    ; binary "**" (neg (app (int 1) [ Expr_constant Const_unit ])) (neg (var "y"))
    ]
;;

(* Генератор выдаёт формы, на которых парсеры чаще всего ошибаются. *)
let () =
  let rand = Random.State.make [| 4 |] in
  let trees =
    QCheck.Gen.generate ~rand ~n:2000 (Generate.structure Generate.core_identifiers)
  in
  let bindings =
    List.concat_map
      (List.concat_map (fun (Structure_value (_, first, rest)) -> first :: rest))
      trees
  in
  let in_expressions predicate =
    List.exists (fun binding -> exists predicate binding.binding_expression) bindings
  in
  let in_bindings predicate = List.exists predicate bindings in
  let operators = List.concat Pprint.binary_operators in
  List.iter
    (fun (form, present) ->
       if not present then fail "генератор не выдал ни разу в 2000 программах: %s" form)
    [ ( "минус от применения константы"
      , in_expressions (function
          | Expr_app (Expr_ident "~-", Expr_app (Expr_constant (Const_int _), _)) -> true
          | _ -> false) )
    ; ( "двойной минус"
      , in_expressions (function
          | Expr_app (Expr_ident "~-", Expr_app (Expr_ident "~-", _)) -> true
          | _ -> false) )
    ; ( "применение true, false или ()"
      , in_expressions (function
          | Expr_app (Expr_constant (Const_bool _ | Const_unit), _) -> true
          | _ -> false) )
    ; ( "min_int"
      , in_expressions (function
          | Expr_constant (Const_int value) -> value = min_int
          | _ -> false) )
    ; ( "кортеж в паттерне связывания"
      , in_bindings (function
          | { binding_pattern = Pat_tuple _; _ } -> true
          | _ -> false) )
    ; ( "отрицательная константа в паттерне связывания"
      , in_bindings (function
          | { binding_pattern = Pat_constant (Const_int value); _ } -> value < 0
          | _ -> false) )
    ; ( "оператор в паттерне связывания"
      , in_bindings (function
          | { binding_pattern = Pat_var name; _ } -> List.mem name operators
          | _ -> false) )
    ; ( "let, fun или if в ветке then"
      , in_expressions (function
          | Expr_if (_, (Expr_let _ | Expr_fun _ | Expr_if _), _) -> true
          | _ -> false) )
    ; ( "let, fun или if последним элементом кортежа"
      , in_expressions (function
          | Expr_tuple (first, second, rest) ->
            (match List.rev (first :: second :: rest) with
             | (Expr_let _ | Expr_fun _ | Expr_if _) :: _ -> true
             | _ -> false)
          | _ -> false) )
    ]
;;

(* Ассоциативность и уровень оператора свойство видит, только если в выборке
   есть два оператора одного уровня подряд без скобок и операторы соседних
   уровней. Без скобок печатается одна из сторон, какая --- зависит от
   ассоциативности, поэтому вложение одного уровня нужно с обеих. *)
let () =
  let rand = Random.State.make [| 1 |] in
  let trees =
    QCheck.Gen.generate ~rand ~n:1000 (Generate.expression Generate.core_identifiers)
  in
  let operator_of group = function
    | Expr_app (Expr_app (Expr_ident name, _), _) -> List.mem name group
    | _ -> false
  in
  let nested ~left ~right outer inner = function
    | Expr_app (Expr_app (Expr_ident name, left_operand), right_operand)
      when List.mem name outer ->
      (left && operator_of inner left_operand)
      || (right && operator_of inner right_operand)
    | _ -> false
  in
  let present predicate = List.exists (exists predicate) trees in
  let levels = Pprint.binary_operators in
  List.iter
    (fun level ->
       if
         not
           (present (nested ~left:true ~right:false level level)
            && present (nested ~left:false ~right:true level level))
       then fail "в 1000 выражениях нет цепочки %s с обеих сторон" (List.hd level))
    levels;
  let rec adjacent = function
    | outer :: (inner :: _ as rest) -> (outer, inner) :: adjacent rest
    | [] | [ _ ] -> []
  in
  List.iter
    (fun (outer, inner) ->
       if not (present (nested ~left:true ~right:true outer inner))
       then fail "в 1000 выражениях нет %s внутри %s" (List.hd inner) (List.hd outer))
    (adjacent levels)
;;
