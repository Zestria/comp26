[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

[@@@zanuda "-mutability_check"] (* Config is required to use with assignment '<-' *)

type parser_kind =
  | Top_down
  | Bottom_up

let string_of_parser_kind = function
  | Top_down -> "top-down"
  | Bottom_up -> "bottom-up"
;;

let parser_kind_of_string = function
  | "topdown" -> Ok Top_down
  | "menhir" -> Ok Bottom_up
  | s -> Error s
;;

type stop_after =
  | Parsing
  | Anf

let stop_after_of_string = function
  | "parsing" -> Ok Parsing
  | "anf" -> Ok Anf
  | s -> Error s
;;

let string_of_stop_after = function
  | Parsing -> "parsing"
  | Anf -> "anf"
;;

type config =
  { mutable output : string
  ; mutable parser : parser_kind
  ; mutable parser_settings : string option
  ; mutable stop_after : stop_after option
  }

let default_config () =
  { output = "-"; parser = Top_down; parser_settings = None; stop_after = None }
;;

let usage_msg =
  {|
SchonfinkelML: miniML compiler.
usage: schonml [OPTIONS] <input-file>"

FORMAT:
For --parser-settings you can pass the following format of argument string:
  "Entry1; Entry2; ...; EntryN"

Each entry has the following form:
  (left|right|nonassoc|prefix) <number> <operations>,
where
  <number> is a number which is precedence,
  <operations> is operation which property you want to overwrite

|}
;;

let flag_specs cfg =
  [ ( "-o"
    , Arg.String (fun f -> cfg.output <- f)
    , " <file>  write output to <file>, \"-\" means stdout (default: -)" )
  ; ( "-parsing"
    , Arg.String
        (fun s ->
          match parser_kind_of_string s with
          | Ok kind -> cfg.parser <- kind
          | Error s ->
            raise
              (Arg.Bad
                 (Printf.sprintf
                    "unknown parser in -parsing: %s (expected topdown, menhir)"
                    s)))
    , " <kind>  choose the parser: topdown (default), menhir" )
  ; "--bottom-up", Arg.Unit (fun () -> cfg.parser <- Bottom_up), "Use LR(1) parser"
  ; ( "--top-down"
    , Arg.Unit (fun () -> cfg.parser <- Top_down)
    , "Use recursive-descent parser, can accept parser settings" )
  ; ( "--parser-settings"
    , Arg.String
        (fun settings ->
          cfg.parser_settings <- Some settings;
          cfg.parser <- Top_down)
    , " <settings>  Pass settings to top down parser, see FORMAT for more info" )
  ; ( "--stop-after"
    , Arg.String
        (fun s ->
          match stop_after_of_string s with
          | Ok stage -> cfg.stop_after <- Some stage
          | Error s ->
            raise
              (Arg.Bad
                 (Printf.sprintf
                    "unknown stage in --stop-after: %s (expected %s)"
                    s
                    (String.concat ", " (List.map string_of_stop_after [ Parsing; Anf ])))))
    , " <stage>  stop after <stage>: parsing, anf" )
  ]
;;

let make_specs cfg =
  let base = flag_specs cfg in
  let help_doc = "  show this help message and exit" in
  let usage () =
    Arg.usage (Arg.align (("-h", Arg.Unit (fun () -> ()), help_doc) :: base)) usage_msg;
    exit 0
  in
  ("-h", Arg.Unit usage, help_doc) :: base
;;
