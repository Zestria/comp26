[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Command-line driver: MiniML source -> RISC-V assembly for the MMML runtime.
   Pipeline: parse -> closure conversion -> lambda lifting -> ANF -> asm. *)

type parser =
  | Topdown
  | Menhir

type diagnostic =
  | Frontend_error of Error_monad.error
  | Closure_error of Middleend.Closure_conversion.error
  | Anf_error of Middleend.Normalize.error

let parse_program = function
  | Topdown -> Topdown_parser.parse_program
  | Menhir -> Parser_menhir.parse_program
;;

let pp_diagnostic fmt = function
  | Frontend_error error -> Error_monad.pp_error fmt error
  | Closure_error error -> Middleend.Closure_conversion.pp_error fmt error
  | Anf_error error -> Middleend.Normalize.pp_error fmt error
;;

let externals =
  "print_int" :: List.map Middleend.Anf.builtin_name Middleend.Anf.builtins
;;

let compile parser source =
  let ( let* ) = Result.bind in
  let* program =
    Result.map_error
      (fun error -> Frontend_error error)
      (parse_program parser source)
  in
  let* program =
    Result.map_error
      (fun error -> Closure_error error)
      (Middleend.Closure_conversion.convert ~externals program)
  in
  let* program =
    Result.map_error
      (fun error -> Anf_error error)
      (Middleend.Normalize.program program)
  in
  Ok (Backend.Asm_conversion.compile program)
;;

let read_source = function
  | None -> In_channel.input_all stdin
  | Some path ->
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> In_channel.input_all channel)
;;

let write_output destination output =
  match destination with
  | "-" ->
    output_string stdout output;
    flush stdout
  | path ->
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () ->
         output_string channel output;
         close_out channel)
;;

let usage =
  "asmgen.exe [-parsing topdown|menhir] [-o file|-] [file]\n\
   Compiles a MiniML program to RISC-V assembly for the MMML runtime.\n\
   Defaults: Menhir, stdout. With options and no file, read stdin."
;;

(* Arg callbacks collect settings before processing starts; these references
   belong to this CLI invocation. *)
[@@@zanuda "-mutability_check"]

let () =
  let parser = ref Menhir in
  let input_file = ref None in
  let output_file = ref "-" in
  let set_input_file path =
    match !input_file with
    | None -> input_file := Some path
    | Some _ -> raise (Arg.Bad "expected at most one input file")
  in
  let options =
    [ ( "-parsing"
      , Arg.Symbol
          ( [ "topdown"; "menhir" ]
          , fun value -> parser := if value = "topdown" then Topdown else Menhir )
      , " Choose a parser (default: menhir)" )
    ; ( "-o"
      , Arg.Set_string output_file
      , "FILE Write the result to FILE; '-' means stdout (default: -)" )
    ]
  in
  try
    Arg.parse_argv Sys.argv options set_input_file usage;
    let source = read_source !input_file in
    match compile !parser source with
    | Ok program -> write_output !output_file (Backend.Codegen.string_of_program program)
    | Error error ->
      Format.eprintf "%a@." pp_diagnostic error;
      exit 1
  with
  | Arg.Bad message ->
    Format.eprintf "%s@." message;
    exit 2
  | Arg.Help message ->
    print_string message;
    flush stdout
  | Sys_error message ->
    Format.eprintf "%s@." message;
    exit 1
;;
