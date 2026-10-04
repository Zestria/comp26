type parsing =
  | Topdown
  | Menhir

type stop_after =
  | Stop_after_parsing
  | Stop_after_anf

type output =
  | To_stdout
  | To_file of string

type t =
  { input_file : string
  ; parsing : parsing
  ; stop_after : stop_after
  ; output : output
  ; operator_settings : string option
  }

let usage =
  "usage: compiler.exe [-parsing topdown|menhir] [--stop-after parsing|anf] \
   [-o <filename>|-o -] [-ops <settings>] <input.ml>"
;;

let help_text =
  {|usage: compiler.exe [options] <input.ml>

A compiler for a subset of OCaml.

positional arguments:
  <input.ml>              name or path of the source file to compile

options:
  -parsing <topdown|menhir>
                          which parser to use (default: topdown)
                            topdown  - recursive-descent (Angstrom) parser
                            menhir   - bottom-up (menhir) parser

  --stop-after <parsing|anf>
                          stop the pipeline after the given stage and print
                          its result (default: parsing)
                            parsing  - print the parsed AST
                            anf      - print the program after ANF conversion

  -o <filename>           write the output to <filename>
  -o -                    write the output to stdout (default)

  -ops <settings>         override operator precedence/associativity/arity;
                          by default the built-in operator table is used.
                          Only for -parsing topdown.
                          <settings> is a ';'-separated list of groups
                          "left|right|prefix <priority> <operators...>",
                          e.g. -ops "left 5 $ ; prefix 1 ~"

  -help, --help           show this help message and exit

example:
  compiler.exe -parsing topdown --stop-after parsing -o - input.ml
|}
;;

let parsing_of_string = function
  | "topdown" -> Ok Topdown
  | "menhir" -> Ok Menhir
  | value -> Error (Printf.sprintf "unknown -parsing value %S (expected topdown or menhir)" value)
;;

let stop_after_of_string = function
  | "parsing" -> Ok Stop_after_parsing
  | "anf" -> Ok Stop_after_anf
  | value ->
    Error (Printf.sprintf "unknown --stop-after value %S (expected \"parsing\" or \"anf\")" value)
;;

type partial =
  { p_input_file : string option
  ; p_parsing : parsing
  ; p_stop_after : stop_after
  ; p_output : output
  ; p_operator_settings : string option
  }

let default_partial =
  { p_input_file = None
  ; p_parsing = Topdown
  ; p_stop_after = Stop_after_parsing
  ; p_output = To_stdout
  ; p_operator_settings = None
  }
;;

type request =
  | Run of t
  | Show_help

let finalize partial =
  match partial.p_input_file with
  | None -> Error ("no input file given\n" ^ usage)
  | Some input_file ->
    Ok
      (Run
         { input_file
         ; parsing = partial.p_parsing
         ; stop_after = partial.p_stop_after
         ; output = partial.p_output
         ; operator_settings = partial.p_operator_settings
         })
;;

let requests_help argv =
  Array.exists
    (fun argument ->
       String.equal argument "-help"
       || String.equal argument "--help"
       || String.equal argument "-h")
    argv
;;

let parse_arguments_impl argv =
  let ( let* ) = Result.bind in
  let length = Array.length argv in
  let need_value flag index =
    if index + 1 >= length
    then Error (Printf.sprintf "option %s requires a value" flag)
    else Ok argv.(index + 1)
  in
  let rec loop partial index =
    if index >= length
    then finalize partial
    else (
      match argv.(index) with
      | "-parsing" ->
        let* value = need_value "-parsing" index in
        let* parsing = parsing_of_string value in
        loop { partial with p_parsing = parsing } (index + 2)
      | "--stop-after" ->
        let* value = need_value "--stop-after" index in
        let* stop_after = stop_after_of_string value in
        loop { partial with p_stop_after = stop_after } (index + 2)
      | "-o" ->
        let* value = need_value "-o" index in
        let output = if String.equal value "-" then To_stdout else To_file value in
        loop { partial with p_output = output } (index + 2)
      | "-ops" ->
        let* value = need_value "-ops" index in
        loop { partial with p_operator_settings = Some value } (index + 2)
      | argument when String.length argument > 0 && Char.equal argument.[0] '-' ->
        Error (Printf.sprintf "unknown option %S\n%s" argument usage)
      | "" -> Error "empty input file name"
      | file ->
        (match partial.p_input_file with
         | Some first ->
           Error
             (Printf.sprintf "input file already given (%S), unexpected argument %S" first file)
         | None -> loop { partial with p_input_file = Some file } (index + 1)))
  in
  loop default_partial 1
;;

let parse_arguments argv = if requests_help argv then Ok Show_help else parse_arguments_impl argv
