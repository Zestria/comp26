[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend
open Ast

let variable text = Var (Test_support.identifier text)
let name = Test_support.operator
let binary text left right = Apply (Var (name text), left, [ right ])
let show program = Format.asprintf "%a" Ast_print.program program

let check expected source =
  match Parser.parse_program_with_operators source with
  | Ok actual -> Test_support.equal ~print:show ~expected actual
  | Error error -> QCheck.Test.fail_reportf "Unexpected parse error: %s" error
;;

let infix_pairs =
  let settings =
    List.concat_map
      (fun precedence -> [ "infixl", precedence; "infixr", precedence ])
      (List.init 10 Fun.id)
  in
  List.concat_map
    (fun (left_assoc, lp) ->
       List.map
         (fun (right_assoc, rp) ->
            let source =
              Printf.sprintf
                "%s %d ( +++ );; %s %d ( *** );; a +++ b *** c"
                left_assoc
                lp
                right_assoc
                rp
            in
            QCheck.Test.make ~name:source ~count:1 QCheck.unit (fun () ->
              if lp = rp && left_assoc <> right_assoc
              then (
                match Parser.parse_program_with_operators source with
                | Error _ -> true
                | Ok actual ->
                  QCheck.Test.fail_reportf
                    "Expected ambiguous operator chain to be rejected; got %s"
                    (show actual))
              else (
                let a, b, c = variable "a", variable "b", variable "c" in
                let expected =
                  if lp < rp || (lp = rp && left_assoc = "infixr")
                  then binary "+++" a (binary "***" b c)
                  else binary "***" (binary "+++" a b) c
                in
                check [ Evaluation expected ] source)))
         settings)
    settings
;;

let prefix_arity =
  let generator =
    QCheck.Gen.(pair (int_range 1 64) (oneof_list [ "+"; "!"; "**"; "||"; "mod"; "##" ]))
  in
  let source (arity, operator) =
    Printf.sprintf
      "prefix %d ( %s );; %s %s"
      arity
      operator
      operator
      (String.concat " " (List.init arity (fun i -> "x" ^ string_of_int i)))
  in
  QCheck.Test.make
    ~name:"declared prefix arity preserves every operand in order"
    ~count:1000
    (QCheck.make ~print:source generator)
    (fun ((arity, operator) as input) ->
       let expected =
         Apply
           ( Var (name operator)
           , variable "x0"
           , List.init (arity - 1) (fun i -> variable ("x" ^ string_of_int (i + 1))) )
       in
       check [ Evaluation expected ] (source input))
;;

let () = QCheck_runner.run_tests_main (prefix_arity :: infix_pairs)
