[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let externals = List.map Middleend.Anf.builtin_name Middleend.Anf.builtins @ [ "print_int" ]

let parse source =
  match Topdown_parser.parse_program source with
  | Ok program -> Ok program
  | Error _ ->
    (match Topdown_parser.parse_expression source with
     | Ok expression ->
       Ok [ Ast.Definition (Ast.Nonrecursive, Ast.PWildcard, expression) ]
     | Error error -> Error (Error_monad.show_error error))

let compile source =
  match parse source with
  | Error message -> Error message
  | Ok program ->
    let program = Middleend.Lambda_lifting.lift_program program in
    (match Middleend.Closure_conversion.convert ~externals program with
     | Error error ->
       Error (Format.asprintf "%a" Middleend.Closure_conversion.pp_error error)
     | Ok program ->
       (match Middleend.Normalize.program program with
        | Error error -> Error (Format.asprintf "%a" Middleend.Normalize.pp_error error)
        | Ok anf -> Ok (Backend.Asm_conversion.compile anf)))
;;

let () =
  let source =
    if Array.length Sys.argv > 1
    then In_channel.with_open_bin Sys.argv.(1) In_channel.input_all
    else In_channel.input_all stdin
  in
  match compile source with
  | Ok program -> print_string (Backend.Codegen.string_of_program program)
  | Error message -> prerr_endline message
;;