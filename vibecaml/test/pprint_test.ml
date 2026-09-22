[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

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

let assert_equal expected actual =
  if not (String.equal expected actual)
  then fail "ожидалось:\n%s\nполучено:\n%s" expected actual
;;

let reparses print parsers printed tree =
  List.iter
    (fun (name, parse) ->
       match parse printed with
       | Ok reparsed when reparsed = tree -> ()
       | Ok reparsed -> fail "%s: %s\nразобрано как\n%s" name printed (print reparsed)
       | Error message -> fail "%s: %s\n%s" name message printed)
    parsers
;;

let check print parsers expected tree =
  let printed = print tree in
  assert_equal expected printed;
  reparses print parsers printed tree
;;

(* Дерево ядра печатается точно так, и OCaml и оба наших парсера разбирают вывод
   обратно в то же дерево. *)
let check_expression =
  check
    Pprint.string_of_expression
    [ "ocaml", Reparse.expression
    ; "menhir", Menhir_frontend.parse_expr
    ; "rd", Rd_parser.parse_expr
    ]
;;

(* Шире ядра: такие операторы принимает только OCaml. *)
let check_wide_expression =
  check Pprint.string_of_expression [ "ocaml", Reparse.expression ]
;;

let check_structure =
  check
    Pprint.string_of_structure
    [ "ocaml", Reparse.structure
    ; "menhir", Menhir_frontend.parse_structure
    ; "rd", Rd_parser.parse_structure
    ]
;;

let () =
  check_expression "1 + 2" (binary "+" (int 1) (int 2));
  check_expression "m * n" (binary "*" (var "m") (var "n"));
  check_expression "n - 1" (binary "-" (var "n") (int 1));
  check_expression "n <= 1" (binary "<=" (var "n") (int 1));
  check_expression "a mod b" (binary "mod" (var "a") (var "b"));
  check_expression "f (-1)" (app (var "f") [ int (-1) ]);
  check_expression
    "1, true, ()"
    (Expr_tuple (int 1, Expr_constant (Const_bool true), [ Expr_constant Const_unit ]));
  check_expression
    "fun (-1) _ (a, b) -> a"
    (Expr_fun
       ( Pat_constant (Const_int (-1))
       , [ Pat_wildcard; Pat_tuple (Pat_var "a", Pat_var "b", []) ]
       , var "a" ));
  check_expression
    "let rec f x = g x and g y = y in if true then f 1 else 0"
    (Expr_let
       ( Recursive
       , binding "f" (Expr_fun (Pat_var "x", [], app (var "g") [ var "x" ]))
       , [ binding "g" (Expr_fun (Pat_var "y", [], var "y")) ]
       , Expr_if (Expr_constant (Const_bool true), app (var "f") [ int 1 ], int 0) ));
  check_expression
    "(true) 1 ((()) 2) ((false) 3)"
    (app
       (Expr_app (Expr_constant (Const_bool true), int 1))
       [ Expr_app (Expr_constant Const_unit, int 2)
       ; Expr_app (Expr_constant (Const_bool false), int 3)
       ]);
  check_expression
    "let ( + ) = ( - ) in 1 + 2"
    (Expr_let (Non_recursive, binding "+" (var "-"), [], binary "+" (int 1) (int 2)));
  (* Сахар [let f x = ...] --- только когда первый параметр переменная. *)
  check_expression
    "let f = fun _ x -> x in f"
    (Expr_let
       ( Non_recursive
       , binding "f" (Expr_fun (Pat_wildcard, [ Pat_var "x" ], var "x"))
       , []
       , var "f" ));
  (* Скобки --- только там, где без них дерево другое. *)
  check_expression "(1 + 2) * 3" (binary "*" (binary "+" (int 1) (int 2)) (int 3));
  check_expression "1 - (2 - 3)" (binary "-" (int 1) (binary "-" (int 2) (int 3)));
  check_expression "(2 ** 3) ** 4" (binary "**" (binary "**" (int 2) (int 3)) (int 4));
  check_expression "2 ** 3 ** 4" (binary "**" (int 2) (binary "**" (int 3) (int 4)));
  check_wide_expression "( @@ ) f x" (binary "@@" (var "f") (var "x"));
  check_expression "-x ** 2" (binary "**" (app (var "~-") [ var "x" ]) (int 2));
  check_expression "-(x ** 2)" (app (var "~-") [ binary "**" (var "x") (int 2) ]);
  check_expression
    "- -f x"
    (app (var "~-") [ app (var "~-") [ app (var "f") [ var "x" ] ] ]);
  check_wide_expression "( ~- ) 1" (app (var "~-") [ int 1 ]);
  check_expression "1 + -1" (binary "+" (int 1) (int (-1)));
  check_expression
    "(fun x -> x) (if a then b else c)"
    (app (Expr_fun (Pat_var "x", [], var "x")) [ Expr_if (var "a", var "b", var "c") ]);
  check_expression
    "if a then b, c else d, e"
    (Expr_if
       (var "a", Expr_tuple (var "b", var "c", []), Expr_tuple (var "d", var "e", [])));
  check_expression
    "(let x = 1 in x), fun y -> y, y"
    (Expr_tuple
       ( Expr_let (Non_recursive, binding "x" (int 1), [], var "x")
       , Expr_fun (Pat_var "y", [], Expr_tuple (var "y", var "y", []))
       , [] ));
  check_expression
    "let (a, b), c = x in a"
    (Expr_let
       ( Non_recursive
       , { binding_pattern =
             Pat_tuple (Pat_tuple (Pat_var "a", Pat_var "b", []), Pat_var "c", [])
         ; binding_expression = var "x"
         }
       , []
       , var "a" ))
;;

(* Краевые случаи, которые свойство может не заметить: лексические склейки,
   свёртка минуса перед константой, min_int рядом с известным отличием ADR. *)
let () =
  let x = var "x"
  and y = var "y"
  and f = var "f" in
  let neg operand = app (var "~-") [ operand ] in
  let if_ condition then_branch else_branch =
    Expr_if (condition, then_branch, else_branch)
  in
  (* Минус перед применением константы --- не константа. *)
  check_expression "-1 x" (neg (app (int 1) [ x ]));
  check_expression "(-1) x" (app (int (-1)) [ x ]);
  check_expression "-(-1) x" (neg (app (int (-1)) [ x ]));
  check_expression "-(-4611686018427387904 ** x)" (neg (binary "**" (int min_int) x));
  check_expression "-(-4611686018427387904) x" (neg (app (int min_int) [ x ]));
  (* Пробелы: иначе [--], [<-] и [->-] --- другие токены. *)
  check_expression "x - -1" (binary "-" x (int (-1)));
  check_expression "x < -1" (binary "<" x (int (-1)));
  check_expression "fun _ -> -1" (Expr_fun (Pat_wildcard, [], int (-1)));
  (* [( *] открывает комментарий, [mod] без скобок --- ключевое слово. *)
  check_expression "( * )" (var "*");
  check_expression "( mod )" (var "mod");
  check_expression
    "if f then if x then y else x, y else f"
    (if_ f (if_ x y (Expr_tuple (x, y, []))) f);
  check_expression "if if x then y else f then x else y" (if_ (if_ x y f) x y);
  check_expression
    "let f x (-1) (x, y) _ () true 0 = x in f"
    (Expr_let
       ( Non_recursive
       , binding
           "f"
           (Expr_fun
              ( Pat_var "x"
              , [ Pat_constant (Const_int (-1))
                ; Pat_tuple (Pat_var "x", Pat_var "y", [])
                ; Pat_wildcard
                ; Pat_constant Const_unit
                ; Pat_constant (Const_bool true)
                ; Pat_constant (Const_int 0)
                ]
              , x ))
       , []
       , f ));
  (* Хвост объявления не забирает следующий [let] или [and]. *)
  let item first rest = Structure_value (Non_recursive, first, rest) in
  check_structure
    "let f = fun _ -> x\nlet y = 1"
    [ item (binding "f" (Expr_fun (Pat_wildcard, [], x))) []
    ; item (binding "y" (int 1)) []
    ];
  check_structure
    "let y = let x = 1 in x\nlet f = y"
    [ item (binding "y" (Expr_let (Non_recursive, binding "x" (int 1), [], x))) []
    ; item (binding "f" y) []
    ];
  check_structure
    "let y = f x\nlet x = 1"
    [ item (binding "y" (app f [ x ])) []; item (binding "x" (int 1)) [] ];
  check_structure
    "let f = fun () -> let y = x in y and y = 1"
    [ item
        (binding
           "f"
           (Expr_fun
              (Pat_constant Const_unit, [], Expr_let (Non_recursive, binding "y" x, [], y))))
        [ binding "y" (int 1) ]
    ];
  check_structure
    "let ( + ) x y = x - y"
    [ item (binding "+" (Expr_fun (Pat_var "x", [ Pat_var "y" ], binary "-" x y))) [] ]
;;

let () =
  check_structure
    "let rec fack n k = if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))\n\
     let rec fibk n k = if n < 2 then k n else fibk (n - 1) (fun a -> fibk (n - 2) (fun \
     b -> k (a + b)))\n\
     let main = fack 5 (fun x -> x), fibk 10 (fun x -> x)"
    [ fack; fibk; main ]
;;

(* Свойство: любое дерево печатается в текст, который OCaml разбирает обратно в
   то же дерево. Имена шире ядра: принтер обязан экранировать любые операторы. *)
let () =
  let round_trip print reparse =
    fun tree ->
    let printed = print tree in
    match reparse printed with
    | Ok reparsed -> reparsed = tree
    | Error message -> QCheck.Test.fail_reportf "%s\n%s" message printed
  in
  Generate.check
    [ QCheck.Test.make
        ~count:1000
        ~name:"выражение переразбирается в то же дерево"
        (QCheck.make
           ~print:Pprint.string_of_expression
           ~shrink:Generate.shrink_expression
           (Generate.expression Generate.wide_identifiers))
        (round_trip Pprint.string_of_expression Reparse.expression)
    ; QCheck.Test.make
        ~count:300
        ~name:"программа переразбирается в то же дерево"
        (QCheck.make
           ~print:Pprint.string_of_structure
           ~shrink:Generate.shrink_structure
           (Generate.structure Generate.wide_identifiers))
        (round_trip Pprint.string_of_structure Reparse.structure)
    ]
;;
