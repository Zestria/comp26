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
  }

let default_config () = { output = "a.out"; parser = Top_down }
let usage_msg = "usage: myprog [OPTIONS] <input-file>"

let flag_specs cfg =
  [ ( "-o"
    , Arg.String (fun f -> cfg.output <- f)
    , "<file>  write output to <file> (default: stdout)" )
  ; ( "--parser"
    , Arg.String
        (function
          | "top-down" -> cfg.parser <- Top_down
          | "bottom-up" -> cfg.parser <- Bottom_up
          | s ->
            raise
              (Arg.Bad
                 (Printf.sprintf
                    "invalid value for --parser: %S (expected 'top-down' or 'bottom-up')"
                    s)))
    , "KIND   parser to use: 'top-down' (default) or 'bottom-up'" )
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
