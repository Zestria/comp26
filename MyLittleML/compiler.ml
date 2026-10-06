[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Front-end driver for MyLittleML (Task 1).

    Reads a program written in an OCaml subset, parses it with one of the
    two parsers (top-down Angstrom parser or bottom-up menhir parser) and
    prints the resulting AST.

    The backend (code generation) is not implemented yet and comes in
    later tasks. *)

type opts = { parser_kind : Parsing.Parse.parser_kind; sticky : bool }

let usage =
  {|Usage: compiler.exe [OPTIONS] [FILE]
Front-end for MyLittleML: parse an OCaml subset and print the AST.

Options:
  --parser topdown|bottomup   choose the parser (default: topdown)
  --sticky                    enable sticky-keywords mode (top-down only)
  --ast | --dump-ast          print the parsed AST (default action)
  --help                      display this help and exit

If FILE is omitted, the program is read from standard input.
|}

let fail_usage msg =
  prerr_endline ("compiler: " ^ msg);
  prerr_endline "Try 'compiler.exe --help' for more information.";
  exit 2
;;

let parse_args argv =
  let parser_kind = ref Parsing.Parse.Topdown in
  let sticky = ref false in
  let file = ref None in
  let rec loop = function
    | [] -> ()
    | "--parser" :: k :: rest ->
      (match k with
       | "topdown" -> parser_kind := Parsing.Parse.Topdown
       | "bottomup" -> parser_kind := Parsing.Parse.Bottomup
       | _ -> fail_usage ("unknown parser: " ^ k));
      loop rest
    | "--parser" :: [] -> fail_usage "option '--parser' requires an argument"
    | "--sticky" :: rest ->
      sticky := true;
      loop rest
    | ("--ast" | "--dump-ast") :: rest -> loop rest
    | "--help" :: _ ->
      print_string usage;
      exit 0
    | a :: rest when String.length a > 0 && a.[0] = '-' ->
      fail_usage ("unrecognized option: " ^ a)
    | f :: rest ->
      (match !file with
       | None -> file := Some f
       | Some _ -> fail_usage "too many arguments");
      loop rest
  in
  loop argv;
  (!file, { parser_kind = !parser_kind; sticky = !sticky })
;;

let read_input = function
  | None -> In_channel.input_all In_channel.stdin
  | Some path -> (
    match In_channel.open_bin path with
    | ch ->
      Fun.protect
        (fun () -> In_channel.input_all ch)
        ~finally:(fun () -> In_channel.close ch)
    | exception Sys_error msg ->
      prerr_endline ("compiler: cannot open file: " ^ msg);
      exit 1)
;;

let run opts source =
  let popts = { Parsing.Parse.sticky = opts.sticky } in
  match Parsing.Parse.parse opts.parser_kind popts source with
  | Ok prog -> print_endline (Parsing.Ast.show_prog prog)
  | Error msg ->
    prerr_endline ("compiler: " ^ msg);
    exit 1
;;

let () =
  let argv = List.tl (Array.to_list Sys.argv) in
  let file, opts = parse_args argv in
  let source = read_input file in
  run opts source
;;