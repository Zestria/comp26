[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open QCheck

let print_expr = Ast.show_expr
let print_program = Ast.show_program

let expr_gen = QCheck.Gen.sized_size (QCheck.Gen.int_bound 30) Ast.gen_expr_sized

let definition_gen =
  QCheck.Gen.map
    (fun (flag, pattern, expression) -> Ast.Definition (flag, pattern, expression))
    (QCheck.Gen.triple Ast.gen_rec_flag Ast.gen_pattern expr_gen)
;;

let program_gen = QCheck.Gen.list_size (QCheck.Gen.int_bound 5) definition_gen

let expr_round_trip =
  Test.make
    ~name:"pretty-print then parse expression"
    ~count:1_000
    (QCheck.make expr_gen ~print:print_expr)
    (fun expression ->
       match Topdown_parser.parse_expression (Pretty_printer.expr_to_string expression) with
       | Ok parsed -> parsed = expression
       | Error _ -> false)
;;

let program_round_trip =
  Test.make
    ~name:"pretty-print then parse program"
    ~count:1_000
    (QCheck.make program_gen ~print:print_program)
    (fun program ->
       match Topdown_parser.parse_program (Pretty_printer.program_to_string program) with
       | Ok parsed -> parsed = program
       | Error _ -> false)
;;

let () = exit (QCheck_runner.run_tests [ expr_round_trip; program_round_trip ])
