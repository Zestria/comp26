[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let fail message =
  prerr_endline ("compiler: " ^ message);
  exit 1
;;

let read_file path =
  try Ok (In_channel.with_open_bin path In_channel.input_all) with
  | Sys_error message -> Error message
;;

let write_output (output : Cli.output) text =
  match output with
  | To_stdout ->
    print_string text;
    Ok ()
  | To_file path ->
    (try
       Ok (Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc text))
     with
     | Sys_error message -> Error message)
;;

let build_table = function
  | None -> Ok Syntax.default_table
  | Some settings -> Op_settings.table_of_string settings
;;

let strip_leading_colon message =
  if String.length message >= 2 && String.equal (String.sub message 0 2) ": "
  then String.sub message 2 (String.length message - 2)
  else message
;;

let parse_structure (cli : Cli.t) source =
  let ( let* ) = Result.bind in
  let parsing_error message =
    Error (Printf.sprintf "%s: parsing error: %s" cli.input_file message)
  in
  match cli.parsing with
  | Topdown ->
    let* table = build_table cli.operator_settings in
    (match Rd_parser.parse_structure ~table source with
     | Ok _ as ok -> ok
     | Error message -> parsing_error (strip_leading_colon message))
  | Menhir ->
    (match cli.operator_settings with
     | Some _ -> Error "-ops is supported only with -parsing topdown"
     | None ->
       (match Menhir_parser.parse_structure source with
        | Ok _ as ok -> ok
        | Error message -> parsing_error message))
;;

let run (cli : Cli.t) =
  let ( let* ) = Result.bind in
  let* source = read_file cli.input_file in
  let* structure = parse_structure cli source in
  match cli.stop_after with
  | Stop_after_parsing -> write_output cli.output (Ast.show_structure structure ^ "\n")
  | Stop_after_anf ->
    let* program = Anf_conv.convert structure in
    write_output cli.output (Anf_pprint.string_of_program program)
  | Stop_after_cc ->
    let* program = Anf_conv.convert structure in
    write_output
      cli.output
      (Anf_pprint.string_of_program (Closure_conversion.program program))
  | Stop_after_ll ->
    let* program = Anf_conv.convert structure in
    let program = Closure_conversion.program program in
    write_output
      cli.output
      (Anf_pprint.string_of_program (Lambda_lifting.program program))
  | Stop_after_riscv ->
    let* program = Anf_conv.convert structure in
    let program = Closure_conversion.program program in
    let program = Lambda_lifting.program program in
    let asm = Codegen.gen_program program in
    write_output cli.output (Riscv.pp_asm asm)
;;

let () =
  match Cli.parse_arguments Sys.argv with
  | Error message -> fail message
  | Ok Cli.Show_help -> print_string Cli.help_text
  | Ok (Cli.Run cli) ->
    (match run cli with
     | Ok () -> ()
     | Error message -> fail message)
;;
