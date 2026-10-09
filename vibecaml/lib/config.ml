[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type parser =
  | Recursive_descent
  | Menhir

type stage =
  | Parsing
  | Rename
  | Closure_conversion
  | Lambda_lifting
  | Anf

type extensions = Parser_intf.extensions

type t =
  { parser : parser
  ; extensions : extensions
  ; stop_after : stage option
  ; dump_ast : bool
  ; dump_pretty_printed : bool
  ; input_file : string option
  ; output_file : string option
  }

let default_extensions : extensions =
  { glued_keywords = false; operator_declarations = false }
;;

let default =
  { parser = Recursive_descent
  ; extensions = default_extensions
  ; stop_after = None
  ; dump_ast = false
  ; dump_pretty_printed = false
  ; input_file = None
  ; output_file = None
  }
;;

let parser_of_string = function
  | "topdown" -> Ok Recursive_descent
  | "menhir" -> Ok Menhir
  | value -> Error ("неизвестный парсер: " ^ value ^ " (ожидается topdown или menhir)")
;;

let stage_of_string = function
  | "parsing" -> Ok Parsing
  | "rename" -> Ok Rename
  | "cc" -> Ok Closure_conversion
  | "ll" -> Ok Lambda_lifting
  | "anf" -> Ok Anf
  | value ->
    Error ("неизвестная стадия: " ^ value ^ " (ожидается parsing, rename, cc, ll или anf)")
;;

(* Дамп печатает результат разбора, поэтому без --stop-after он сам
   останавливает конвейер после разбора. *)
let check_dumps config =
  match config.stop_after with
  | _ when not (config.dump_ast || config.dump_pretty_printed) -> Ok config
  | None -> Ok { config with stop_after = Some Parsing }
  | Some Parsing -> Ok config
  | Some (Rename | Closure_conversion | Lambda_lifting | Anf) ->
    Error "--dump-ast и --dump-pp работают только с --stop-after parsing"
;;

let is_option argument = String.length argument > 1 && argument.[0] = '-'

let parse_arguments arguments =
  let length = Array.length arguments in
  let ( let* ) = Result.bind in
  (* Следующий флаг не может быть значением: иначе [-o --stop-after ll]
     молча записал бы вывод в файл [--stop-after]. *)
  let value_after flag index =
    if index + 1 >= length || is_option arguments.(index + 1)
    then Error ("после " ^ flag ^ " ожидается значение")
    else Ok arguments.(index + 1)
  in
  let rec loop config index =
    if index >= length
    then check_dumps config
    else (
      match arguments.(index) with
      | "-parsing" ->
        let* value = value_after "-parsing" index in
        let* parser = parser_of_string value in
        loop { config with parser } (index + 2)
      | "--stop-after" ->
        let* value = value_after "--stop-after" index in
        let* stage = stage_of_string value in
        loop { config with stop_after = Some stage } (index + 2)
      | "-o" ->
        let* file = value_after "-o" index in
        (match file, config.output_file with
         | "", _ -> Error "пустое имя выходного файла"
         | _, Some first ->
           Error ("выходной файл уже задан: " ^ first ^ ", лишний: " ^ file)
         | _, None -> loop { config with output_file = Some file } (index + 2))
      | "--ext-glued-keywords" ->
        let extensions = { config.extensions with glued_keywords = true } in
        loop { config with extensions } (index + 1)
      | "--ext-operator-declarations" ->
        let extensions = { config.extensions with operator_declarations = true } in
        loop { config with extensions } (index + 1)
      | "--dump-ast" -> loop { config with dump_ast = true } (index + 1)
      | "--dump-pp" -> loop { config with dump_pretty_printed = true } (index + 1)
      | option when is_option option -> Error ("неизвестный аргумент: " ^ option)
      | "" -> Error "пустое имя входного файла"
      | file ->
        (match config.input_file with
         | Some first -> Error ("входной файл уже задан: " ^ first ^ ", лишний: " ^ file)
         | None -> loop { config with input_file = Some file } (index + 1)))
  in
  loop default 1
;;
