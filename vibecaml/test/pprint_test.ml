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

(* Вывод печатается точно так, и OCaml разбирает его обратно в то же дерево. *)
let check_expression expected expression =
  let printed = Pprint.string_of_expression expression in
  assert_equal expected printed;
  match Reparse.expression printed with
  | Ok reparsed when reparsed = expression -> ()
  | Ok _ -> fail "переразбирается в другое дерево: %s" printed
  | Error message -> fail "%s\n%s" message printed
;;

let check_structure expected structure =
  let printed = Pprint.string_of_structure structure in
  assert_equal expected printed;
  match Reparse.structure printed with
  | Ok reparsed when reparsed = structure -> ()
  | Ok _ -> fail "переразбирается в другое дерево:\n%s" printed
  | Error message -> fail "%s\n%s" message printed
;;

let () =
  check_expression "((( + ) 1) 2)" (binary "+" (int 1) (int 2));
  check_expression "((( * ) m) n)" (binary "*" (var "m") (var "n"));
  check_expression "((( - ) n) 1)" (binary "-" (var "n") (int 1));
  check_expression "((( <= ) n) 1)" (binary "<=" (var "n") (int 1));
  check_expression "((( mod ) a) b)" (binary "mod" (var "a") (var "b"));
  check_expression "(f (-1))" (app (var "f") [ int (-1) ]);
  check_expression
    "(1, true, ())"
    (Expr_tuple (int 1, Expr_constant (Const_bool true), [ Expr_constant Const_unit ]));
  check_expression
    "(fun (-1) _ (a, b) -> a)"
    (Expr_fun
       ( Pat_constant (Const_int (-1))
       , [ Pat_wildcard; Pat_tuple (Pat_var "a", Pat_var "b", []) ]
       , var "a" ));
  check_expression
    "(let rec f = (fun x -> (g x)) and g = (fun y -> y) in (if true then (f 1) else 0))"
    (Expr_let
       ( Recursive
       , binding "f" (Expr_fun (Pat_var "x", [], app (var "g") [ var "x" ]))
       , [ binding "g" (Expr_fun (Pat_var "y", [], var "y")) ]
       , Expr_if (Expr_constant (Const_bool true), app (var "f") [ int 1 ], int 0) ));
  check_expression
    "((((true) 1) ((()) 2)) ((false) 3))"
    (app
       (Expr_app (Expr_constant (Const_bool true), int 1))
       [ Expr_app (Expr_constant Const_unit, int 2)
       ; Expr_app (Expr_constant (Const_bool false), int 3)
       ]);
  check_expression
    "(let ( + ) = ( - ) in ((( + ) 1) 2))"
    (Expr_let (Non_recursive, binding "+" (var "-"), [], binary "+" (int 1) (int 2)))
;;

let () =
  check_structure
    "let rec fack = (fun n k -> (if ((( <= ) n) 1) then (k 1) else ((fack ((( - ) n) 1)) \
     (fun m -> (k ((( * ) m) n))))))\n\
     let rec fibk = (fun n k -> (if ((( < ) n) 2) then (k n) else ((fibk ((( - ) n) 1)) \
     (fun a -> ((fibk ((( - ) n) 2)) (fun b -> (k ((( + ) a) b))))))))\n\
     let main = (((fack 5) (fun x -> x)), ((fibk 10) (fun x -> x)))"
    [ fack; fibk; main ]
;;

(* Свойство: любое дерево печатается в текст, который OCaml разбирает обратно в
   то же дерево. *)
module Generate = struct
  open QCheck.Gen

  let identifier =
    oneof_list
      [ "x"
      ; "y"
      ; "f"
      ; "k"
      ; "fack"
      ; "x'"
      ; "_a"
      ; "x1"
      ; "+"
      ; "-"
      ; "*"
      ; "<"
      ; "<="
      ; "="
      ; "&&"
      ; "||"
      ; "**"
      ; "*."
      ; "~-"
      ; "!"
      ; "!="
      ; ":="
      ; "@@"
      ; "##"
      ; "mod"
      ; "lsl"
      ; "or"
      ]
  ;;

  let constant =
    oneof
      [ map
          (fun value -> Const_int value)
          (oneof [ int_small; map ( ~- ) int_small; oneof_list [ min_int; max_int ] ])
      ; map (fun value -> Const_bool value) bool
      ; return Const_unit
      ]
  ;;

  let short_list element = list_size (int_range 0 2) element

  let pattern =
    sized_size (int_bound 3)
    @@ fix (fun self size ->
      let leaf =
        oneof
          [ return Pat_wildcard
          ; map (fun name -> Pat_var name) identifier
          ; map (fun value -> Pat_constant value) constant
          ]
      in
      if size = 0
      then leaf
      else (
        let child = self (size / 2) in
        oneof
          [ leaf
          ; map3 (fun a b rest -> Pat_tuple (a, b, rest)) child child (short_list child)
          ]))
  ;;

  let rec_flag = oneof_list [ Recursive; Non_recursive ]

  let binding expression =
    map2
      (fun binding_pattern binding_expression -> { binding_pattern; binding_expression })
      pattern
      expression
  ;;

  let expression =
    sized_size (int_bound 20)
    @@ fix (fun self size ->
      let leaf =
        oneof
          [ map (fun value -> Expr_constant value) constant
          ; map (fun name -> Expr_ident name) identifier
          ]
      in
      if size = 0
      then leaf
      else (
        let child = self (size / 2) in
        oneof
          [ leaf
          ; map2 (fun f a -> Expr_app (f, a)) child child
          ; map3 (fun c t e -> Expr_if (c, t, e)) child child child
          ; map3 (fun a b rest -> Expr_tuple (a, b, rest)) child child (short_list child)
          ; map3
              (fun first rest body -> Expr_fun (first, rest, body))
              pattern
              (short_list pattern)
              child
          ; map2
              (fun (flag, first) (rest, body) -> Expr_let (flag, first, rest, body))
              (pair rec_flag (binding child))
              (pair (short_list (binding child)) child)
          ]))
  ;;

  let structure =
    list_size
      (int_range 1 3)
      (map3
         (fun flag first rest -> Structure_value (flag, first, rest))
         rec_flag
         (binding expression)
         (short_list (binding expression)))
  ;;
end

(* Контрпример сжимаем до поддерева: упавшее поддерево обычно падает и само. *)
module Shrink = struct
  let children = function
    | Expr_constant _ | Expr_ident _ -> []
    | Expr_let (_, first, rest, body) ->
      body :: List.map (fun binding -> binding.binding_expression) (first :: rest)
    | Expr_fun (_, _, body) -> [ body ]
    | Expr_app (function_, argument) -> [ function_; argument ]
    | Expr_if (condition, then_branch, else_branch) ->
      [ condition; then_branch; else_branch ]
    | Expr_tuple (first, second, rest) -> first :: second :: rest
  ;;

  let expression expression yield = List.iter yield (children expression)
end

(* Seed случайный, чтобы каждый прогон проверял новые деревья; при падении он
   печатается, и прогон повторяется через QCHECK_SEED=<seed>. *)
let () =
  let seed =
    match Sys.getenv_opt "QCHECK_SEED" with
    | Some seed -> int_of_string seed
    | None ->
      Random.self_init ();
      Random.bits ()
  in
  let round_trip print reparse =
    fun tree ->
    let printed = print tree in
    match reparse printed with
    | Ok reparsed -> reparsed = tree
    | Error message -> QCheck.Test.fail_reportf "%s\n%s" message printed
  in
  let tests =
    [ QCheck.Test.make
        ~count:1000
        ~name:"выражение переразбирается в то же дерево"
        (QCheck.make
           ~print:Pprint.string_of_expression
           ~shrink:Shrink.expression
           Generate.expression)
        (round_trip Pprint.string_of_expression Reparse.expression)
    ; QCheck.Test.make
        ~count:300
        ~name:"программа переразбирается в то же дерево"
        (QCheck.make ~print:Pprint.string_of_structure Generate.structure)
        (round_trip Pprint.string_of_structure Reparse.structure)
    ]
  in
  let rand = Random.State.make [| seed |] in
  List.iter
    (fun test ->
       try QCheck.Test.check_exn ~rand test with
       | error -> fail "QCHECK_SEED=%d\n%s" seed (Printexc.to_string error))
    tests
;;
