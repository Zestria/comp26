(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Vibecaml

(* Точка входа компилятора: читает программу из файла или stdin, разбирает
   выбранным парсером, прогоняет конвейер до нужной стадии и печатает
   результат в stdout или в файл. *)

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
let show_parsed config structure =
  let { Config.dump_ast; dump_pretty_printed; _ } = config in
  let tree = if dump_ast then Format.asprintf "%a@." Ast.pp_structure structure else "" in
  let program =
    if dump_pretty_printed || not dump_ast
    then Format.asprintf "%a@." Pprint.pp_structure structure
    else ""
  in
  tree ^ program
;;

let show structure = Format.asprintf "%a@." Pprint.pp_structure structure

(* Конвейер middleend по порядку из #9; на стадии из --stop-after печатаем
   результат и останавливаемся. *)
let compile config parsed =
  let ( let* ) = Result.bind in
  let stop_after stage = config.Config.stop_after = Some stage in
  if stop_after Config.Parsing
  then Ok (show_parsed config parsed)
  else (
    (* TODO(#10): rename и closure conversion, пока тождество. *)
    let converted = parsed in
    if stop_after Config.Closure_conversion
    then Ok (show converted)
    else
      let* lifted = Lambda_lift.lift converted in
      if stop_after Config.Lambda_lifting
      then Ok (show lifted)
      else if stop_after Config.Anf
      then (* TODO(#12): ANF после мержа PR 9. *)
        Error "стадия anf пока не подключена"
      else Ok (show lifted))
;;

let write_output config text =
  match config.Config.output_file with
  | None | Some "-" -> Ok (print_string text)
  | Some file ->
    (try
       Ok (Out_channel.with_open_bin file (fun channel -> output_string channel text))
     with
     | Sys_error message -> Error message)
;;

let run config =
  let ( let* ) = Result.bind in
  let* source = read_source config.Config.input_file in
  let* structure = parse_structure config source in
  Fresh.reset ();
  let* text = compile config structure in
  write_output config text
;;

let () =
  match Result.bind (Config.parse_arguments Sys.argv) run with
  | Error message -> fail message
  | Ok () -> ()
;;
