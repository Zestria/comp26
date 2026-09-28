(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Vibecaml

(* Точка входа компилятора: читает программу из файла или stdin, разбирает
   выбранным парсером и печатает дерево и/или программу принтером. *)

let read_source = function
  | None | Some "-" -> Ok (In_channel.input_all In_channel.stdin)
  | Some file ->
    (try Ok (In_channel.with_open_bin file In_channel.input_all) with
     | Sys_error message -> Error message)
;;

let parse_structure config source =
  match config.Config.parser with
  | Config.Recursive_descent -> Rd_parser.parse_structure config.Config.extensions source
  | Config.Menhir -> Menhir_frontend.parse_structure config.Config.extensions source
;;

let fail message =
  prerr_endline ("vibecaml: " ^ message);
  exit 1
;;

(* Без флагов дампа печатаем программу принтером: такой вывод легко читать и
   сравнивать между парсерами. *)
let print_outputs config structure =
  let { Config.dump_ast; dump_pretty_printed; _ } = config in
  if dump_ast then Format.printf "%a@." Ast.pp_structure structure;
  if dump_pretty_printed || not dump_ast
  then Format.printf "%a@." Pprint.pp_structure structure
;;

let run config =
  let ( let* ) = Result.bind in
  let* source = read_source config.Config.input_file in
  let* structure = parse_structure config source in
  Ok (print_outputs config structure)
;;

let () =
  match Result.bind (Config.parse_arguments Sys.argv) run with
  | Error message -> fail message
  | Ok () -> ()
;;
