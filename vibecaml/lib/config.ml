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
