[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type parser_kind =
  | Top_down
  | Bottom_up

let string_of_parser_kind = function
  | Top_down -> "top-down"
  | Bottom_up -> "bottom-up"
;;

type config =
  { mutable output : string
  ; mutable parser : parser_kind
  ; mutable parser_settings : string option
  }

let default_config () = { output = "a.out"; parser = Top_down; parser_settings = None }

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
    , " <file>  write output to <file> (default: stdout)" )
  ; "--bottom-up", Arg.Unit (fun () -> cfg.parser <- Bottom_up), "Use LR(1) parser"
  ; ( "--top-down"
    , Arg.Unit (fun () -> cfg.parser <- Top_down)
    , "Use recursive-descent parser, can accept parser settings" )
  ; ( "--parser-settings"
    , Arg.Unit (fun () -> cfg.parser <- Top_down)
    , " <settings>  Pass settings to top down parser, see FORMAT for more info" )
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
