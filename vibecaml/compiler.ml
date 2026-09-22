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

(* Расширения пока нигде не реализованы (#5). Флаг, который молча ничего не
   включает, хуже явной ошибки, поэтому отказываем до чтения входа. *)
let check_extensions config =
  let { Config.glued_keywords; operator_declarations } = config.Config.extensions in
  let enabled =
    List.find_opt
      snd
      [ "--ext-glued-keywords", glued_keywords
      ; "--ext-operator-declarations", operator_declarations
      ]
  in
  match enabled, config.Config.parser with
  | None, _ -> Ok ()
  | Some (flag, _), Config.Recursive_descent ->
    Error (Printf.sprintf "расширение %s пока не реализовано" flag)
  | Some (flag, _), Config.Menhir ->
    Error (Printf.sprintf "расширение %s не поддерживается в menhir-парсере" flag)
;;

let fail message =
  prerr_endline ("vibecaml: " ^ message);
  exit 1
;;

let run config =
  let ( let* ) = Result.bind in
  let* () = check_extensions config in
  let* source = read_source config.Config.input_file in
  parse_structure config source
;;

let () =
  match Result.bind (Config.parse_arguments Sys.argv) run with
  | Error message -> fail message
  | Ok structure ->
    (* Пока AST печатается принтером в исходный синтаксис: вывод легко
       читать и сравнивать между парсерами. *)
    Format.printf "%a@." Pprint.pp_structure structure
;;
