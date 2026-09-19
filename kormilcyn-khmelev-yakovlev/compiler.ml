[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Kormilcyn_khmelev_yakovlev_lib

type parser_kind =
  | Comb
  | Menhir

type opts =
  { parser_kind : parser_kind
  ; extended : bool
  ; show_ast : bool
  }

let default_opts = { parser_kind = Comb; extended = false; show_ast = false }

let usage () =
  print_string
    "Usage: compiler.exe [options] [file]\n\
     Reads an expression from <file> (or stdin) and parses it.\n\n\
     Options:\n\
    \  --parser=comb|menhir  Select top-down (default) or bottom-up parser\n\
    \  --extended            Allow reserved words as identifiers (e.g. `let in = 1 in in`); combinator parser only\n\
    \  --ast                 Print the parsed AST\n\
    \  --help                Show this message\n"
;;

let parse_args () =
  let rec loop opts input_file = function
    | [] -> opts, input_file
    | "--help" :: _ ->
      usage ();
      exit 0
    | "--ast" :: rest -> loop { opts with show_ast = true } input_file rest
    | "--extended" :: rest -> loop { opts with extended = true } input_file rest
    | "--parser=comb" :: rest -> loop { opts with parser_kind = Comb } input_file rest
    | "--parser=menhir" :: rest -> loop { opts with parser_kind = Menhir } input_file rest
    | filename :: rest ->
      (match input_file with
       | None -> loop opts (Some filename) rest
       | Some _ ->
         Format.eprintf "Error: multiple input files specified\n";
         exit 1)
  in
  loop default_opts None (List.tl (Array.to_list Sys.argv))
;;

let parse_with_menhir_result text =
  try Result.Ok (Main.parse_with_menhir text) with
  | _ -> Result.Error (`Parsing_error "menhir: syntax error")
;;

let run opts input_text =
  let text = String.trim input_text in
  let result =
    match opts.parser_kind with
    | Comb ->
      if opts.extended
      then Main.parse_with_comb_extended text
      else Main.parse_with_comb text
    | Menhir ->
      if opts.extended
      then (
        Format.eprintf "Error: --extended is only supported by the combinator parser\n";
        exit 1)
      else parse_with_menhir_result text
  in
  match result with
  | Error (`Parsing_error msg) ->
    Format.eprintf "Parser error: %s\n" msg;
    exit 1
  | Ok ast ->
    if opts.show_ast then Format.printf "%a\n" Ast.pp_expr ast;
    print_string (Printast.show ast);
    print_newline ()
;;

let () =
  let opts, input_file = parse_args () in
  let input_text =
    match input_file with
    | Some filename -> In_channel.(with_open_text filename input_all)
    | None -> In_channel.(input_all stdin)
  in
  run opts input_text
;;
