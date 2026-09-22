(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Vibecaml

(* Точка входа компилятора: читает программу из файла или stdin, разбирает
   выбранным парсером и печатает её обратно принтером. *)

let read_source = function
  | None | Some "-" -> Ok (In_channel.input_all In_channel.stdin)
  | Some file ->
    (try Ok (In_channel.with_open_bin file In_channel.input_all) with
     | Sys_error message -> Error message)
;;

let parse_structure config source =
  match config.Config.parser with
  | Config.Recursive_descent -> Rd_parser.parse_structure source
  | Config.Menhir -> Menhir_frontend.parse_structure source
;;

let fail message =
  prerr_endline ("vibecaml: " ^ message);
  exit 1
;;

let () =
  match Config.parse_arguments Sys.argv with
  | Error message -> fail message
  | Ok config ->
    (match read_source config.input_file with
     | Error message -> fail message
     | Ok source ->
       (match parse_structure config source with
        | Error message -> fail message
        | Ok structure ->
          (* Пока AST печатается принтером в исходный синтаксис: вывод легко
             читать и сравнивать между парсерами. *)
          Format.printf "%a@." Pprint.pp_structure structure))
;;
