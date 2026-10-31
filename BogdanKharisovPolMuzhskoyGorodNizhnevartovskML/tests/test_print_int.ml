[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend
open Bkml_middleend
open Bkml_middleend_test_support

let compile source =
  match Pipeline.compile (parsed Parser.parse_program source) with
  | Ok program -> program
  | Error error -> fail source (Pipeline.error_to_string error)
;;

let output source =
  match Anf_eval.lifted_with_output (compile source) with
  | Ok (_, output) -> output
  | Error error -> fail source (Eval_value.error_to_string error)
;;

let case (name, source, expected) =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    equal source expected (output source))
;;

let cases =
  [ "print_int sequences without a newline", "let _ = print_int 1 in print_int 2", "12"
  ; "print_int runs across structure items", "print_int 1;; print_int 2", "12"
  ; ( "print_int arguments evaluate right to left"
    , "let f a b = 0 in f (print_int 1) (print_int 2)"
    , "21" )
  ; ( "print_int tuple fields evaluate right to left"
    , "let t = (print_int 1, print_int 2) in 0"
    , "21" )
  ; "a local binding shadows print_int", "let print_int x = x in print_int 5", ""
  ; "print_int is an ordinary function value", "let p = print_int in p 7", "7"
  ; "print_int prints negative decimals", "let _ = print_int (-5) in 0", "-5"
  ; "an unused print_int result remains effectful", "let x = print_int 3 in 0", "3"
  ; ( "print_int inside a function runs on every call"
    , "let f x = print_int x in let _ = f 9 in f 9"
    , "99" )
  ]
;;

let () = run_tests (List.map case cases)
