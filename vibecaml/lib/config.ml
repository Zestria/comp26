[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type parser =
  | Recursive_descent
  | Menhir

type extensions = Parser_intf.extensions

type t =
  { parser : parser
  ; extensions : extensions
  ; dump_ast : bool
  ; dump_pretty_printed : bool
  ; input_file : string option
  }

let default_extensions : extensions =
  { glued_keywords = false; operator_declarations = false }
;;

let default =
  { parser = Recursive_descent
  ; extensions = default_extensions
  ; dump_ast = false
  ; dump_pretty_printed = false
  ; input_file = None
  }
;;

let parser_of_string = function
  | "rd" -> Ok Recursive_descent
  | "menhir" -> Ok Menhir
  | value -> Error ("неизвестный парсер: " ^ value)
;;

let is_option argument = String.length argument > 1 && argument.[0] = '-'

let parse_arguments arguments =
  let length = Array.length arguments in
  let rec loop config index =
    if index >= length
    then Ok config
    else (
      match arguments.(index) with
      | "--parser" ->
        if index + 1 >= length
        then Error "после --parser ожидается значение"
        else (
          match parser_of_string arguments.(index + 1) with
          | Error _ as error -> error
          | Ok parser -> loop { config with parser } (index + 2))
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
