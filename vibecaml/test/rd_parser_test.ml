[@@@ocaml.text "/*"]

(* Copyright 2026, vibecaml team *)
(* SPDX-License-Identifier: LGPL-3.0-or-later *)
[@@@ocaml.text "/*"]

open Vibecaml

let fail format =
  Printf.ksprintf
    (fun message ->
       prerr_endline message;
       exit 1)
    format
;;

let parsed_expression source =
  match Rd_parser.parse_expr source with
  | Ok expression -> expression
  | Error message -> fail "не удалось разобрать %S: %s" source message
;;

let parsed_structure source =
  match Rd_parser.parse_structure source with
  | Ok structure -> structure
  | Error message -> fail "не удалось разобрать программу: %s" message
;;

let check_expression source expected =
  let actual = Pprint.string_of_expression (parsed_expression source) in
  if not (String.equal actual expected)
  then fail "для %S ожидалось %s, получено %s" source expected actual
;;

let () =
  check_expression "1 + 2 * 3" "((( + ) 1) ((( * ) 2) 3))";
  check_expression "1 - 2 - 3" "((( - ) ((( - ) 1) 2)) 3)";
  check_expression "2 ** 3 ** 4" "((( ** ) 2) ((( ** ) 3) 4))";
  check_expression "f x y" "((f x) y)";
  check_expression
    "let rec f x y = x + y and g = f 1 in g"
    "(let rec f = (fun x y -> ((( + ) x) y)) and g = (f 1) in g)";
  check_expression "if x < 2 then f x else g x" "(if ((( < ) x) 2) then (f x) else (g x))"
;;

let () =
  let source =
    "let rec fack n k = if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))\n"
    ^ "let rec fibk n k = if n < 2 then k n else fibk (n - 1) (fun a -> fibk (n - 2) \
       (fun b -> k (a + b)))"
  in
  match parsed_structure source with
  | [ _; _ ] -> ()
  | _ -> fail "эталонные программы должны дать два объявления"
;;

let () =
  match Rd_parser.parse_expr "let x =" with
  | Error message when String.length message > 0 -> ()
  | Error _ -> fail "пустое сообщение об ошибке"
  | Ok _ -> fail "некорректный ввод не должен разбираться"
;;
