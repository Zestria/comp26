(** Настройки выбора парсера и экспериментального синтаксиса. *)

type parser =
  | Recursive_descent
  | Menhir

type extensions =
  { glued_keywords : bool
  ; operator_declarations : bool
  }

type t =
  { parser : parser
  ; extensions : extensions
  ; dump_ast : bool
  ; dump_pretty_printed : bool
  }

let default_extensions = { glued_keywords = false; operator_declarations = false }

let default =
  { parser = Recursive_descent
  ; extensions = default_extensions
  ; dump_ast = false
  ; dump_pretty_printed = false
  }
;;

(** Разобрать значение флага [--parser]. *)
let parser_of_string = function
  | "rd" -> Ok Recursive_descent
  | "menhir" -> Ok Menhir
  | value -> Error ("неизвестный парсер: " ^ value)
;;

(** Разобрать аргументы командной строки без побочных эффектов. *)
let parse_arguments arguments =
  let length = Array.length arguments in
  let rec loop config index =
    if index = length
    then Ok config
    else (
      match arguments.(index) with
      | "--parser" ->
        if index + 1 = length
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
      | option -> Error ("неизвестный аргумент: " ^ option))
  in
  loop default 1
;;
