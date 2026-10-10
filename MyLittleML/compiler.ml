[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Middle-end driver for MyLittleML (Task 2).

    Reads a program written in an OCaml subset, parses it with one of the
    two parsers (top-down Angstrom parser or bottom-up menhir parser) and
    runs it through the middle-end pipeline: ANF → closure conversion →
    lambda lifting.

    The backend (code generation) is not implemented yet and comes in
    later tasks. *)

type stop_after =
  | Parsing
  | Anf
  | Closure
  | Lambda

type opts =
  { parser_kind : Parsing.Parse.parser_kind
  ; sticky : bool
  ; stop_after : stop_after
  ; output : string option
  }

let usage =
  {|Usage: compiler.exe [OPTIONS] [FILE]
Middle-end for MyLittleML: parse an OCaml subset and run ANF,
closure conversion and lambda lifting.

Options:
  -parsing topdown|menhir    choose the parser (default: topdown;
                             'bottomup' is accepted as an alias of 'menhir')
  --parser topdown|bottomup  legacy alias of -parsing
  --sticky                   enable sticky-keywords mode (top-down only)
  --stop-after parsing|anf|closure-conversion|lambda-lifting
                             stop the pipeline after the given stage
                             (default: run the whole pipeline)
  -o FILE                    write the result to FILE ('-o -' writes
                             to standard output, which is also the default)
  --ast | --dump-ast         legacy: stop after parsing and print the AST
  --help                     display this help and exit

If FILE is omitted, the program is read from standard input.
|}

let fail_usage msg =
  prerr_endline ("compiler: " ^ msg);
  prerr_endline "Try 'compiler.exe --help' for more information.";
  exit 2
;;

let parse_parser_kind s =
  match s with
  | "topdown" -> Parsing.Parse.Topdown
  | "menhir" | "bottomup" -> Parsing.Parse.Bottomup
  | _ -> fail_usage ("unknown parser: " ^ s ^ " (expected 'topdown' or 'menhir')")
;;

let parse_stop_after = function
  | "parsing" -> Parsing
  | "anf" -> Anf
  | "closure-conversion" -> Closure
  | "lambda-lifting" -> Lambda
  | s -> fail_usage ("unknown stage: " ^ s ^ " (expected 'parsing', 'anf', 'closure-conversion' or 'lambda-lifting')")
;;

let parse_args argv =
  let parser_kind = ref Parsing.Parse.Topdown in
  let sticky = ref false in
  let stop_after = ref Lambda in
  let output = ref None in
  let file = ref None in
  let set_parser s =
    parser_kind := parse_parser_kind s
  in
  let rec loop = function
    | [] -> ()
    | ("-parsing" | "--parser") :: k :: rest ->
      set_parser k;
      loop rest
    | ("-parsing" | "--parser") :: [] ->
      fail_usage "option '-parsing' requires an argument ('topdown' or 'menhir')"
    | "--sticky" :: rest ->
      sticky := true;
      loop rest
    | "--stop-after" :: stage :: rest ->
      stop_after := parse_stop_after stage;
      loop rest
    | "--stop-after" :: [] -> fail_usage "option '--stop-after' requires an argument"
    | "-o" :: path :: rest ->
      output := Some path;
      loop rest
    | "-o" :: [] -> fail_usage "option '-o' requires an argument"
    | ("--ast" | "--dump-ast") :: rest ->
      stop_after := Parsing;
      loop rest
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
  !file, { parser_kind = !parser_kind; sticky = !sticky; stop_after = !stop_after; output = !output }
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
  let prog =
    match Parsing.Parse.parse opts.parser_kind popts source with
    | Ok prog -> prog
    | Error msg ->
      prerr_endline ("compiler: " ^ msg);
      exit 1
  in
  let show_anf (stage : stop_after) prog =
    let prog = Middleend.Anf.anf_prog prog in
    let prog =
      match stage with
      | Parsing -> assert false
      | Anf -> prog
      | Closure | Lambda -> Middleend.Closure.convert_prog prog
    in
    let prog =
      match stage with
      | Parsing -> assert false
      | Lambda -> Middleend.Lambda_lift.lift_prog prog
      | Anf | Closure -> prog
    in
    Middleend.Anf.show_aprogram prog
  in
  let output =
    match opts.stop_after with
    | Parsing -> Parsing.Ast.show_prog prog
    | Anf -> show_anf Anf prog
    | Closure -> show_anf Closure prog
    | Lambda -> show_anf Lambda prog
  in
  (match opts.output with
   | None | Some "-" -> print_endline output
   | Some path ->
     (match Out_channel.open_bin path with
      | ch ->
        Fun.protect
          (fun () -> Out_channel.output_string ch (output ^ "\n"))
          ~finally:(fun () -> Out_channel.close ch)
      | exception Sys_error msg ->
        prerr_endline ("compiler: cannot open output file: " ^ msg);
        exit 1))
;;

let () =
  let argv = List.tl (Array.to_list Sys.argv) in
  let file, opts = parse_args argv in
  let source = read_input file in
  run opts source
;;