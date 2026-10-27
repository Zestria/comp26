[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_lib
open Shmel_middleend

type parser_kind =
  | Topdown
  | Menhir

type stage =
  | Parsing
  | Anf_stage
  | Closure_stage
  | Lifting_stage
  | Riscv_stage

type output =
  | Stdout
  | File of string

type opts =
  { parser_kind : parser_kind
  ; extended : bool
  ; show_ast : bool
  ; stop_after : stage
  ; output : output
  ; input : string option
  }

type command =
  | Help
  | Llvm_demo
  | Compile of opts

let default_opts =
  { parser_kind = Topdown
  ; extended = false
  ; show_ast = false
  ; stop_after = Riscv_stage
  ; output = Stdout
  ; input = None
  }
;;

let usage =
  "Usage: compiler.exe [options] [file]\n\
   Compiles an ML program from <file> (or stdin).\n\n\
   Options:\n\
  \  -parsing topdown|menhir   Parser to use: top-down (default) or bottom-up\n\
  \  --stop-after parsing|anf|closure|lifting|riscv  Stop after the stage and print its \
   result (default: riscv)\n\
  \  -o <file>                 Write the result to <file>; '-o -' writes to stdout \
   (default)\n\
  \  --extended                Allow reserved words as identifiers (e.g. `let in = 1 in \
   in`); top-down parser only\n\
  \  --ast                     With '--stop-after parsing', also print the AST\n\
  \  --llvm-demo               Run the LLVM codegen demo instead of compiling (writes \
   out.ll)\n\
  \  --help                    Show this message\n"
;;

let parse_args args =
  let rec loop llvm_demo opts = function
    | [] ->
      (match llvm_demo, opts with
       | true, _ -> Ok Llvm_demo
       | false, { parser_kind = Menhir; extended = true; _ } ->
         Error "--extended is only supported by the top-down parser"
       | false, _ -> Ok (Compile opts))
    | ("--help" | "-help") :: _ -> Ok Help
    | "-parsing" :: "topdown" :: rest ->
      loop llvm_demo { opts with parser_kind = Topdown } rest
    | "-parsing" :: "menhir" :: rest ->
      loop llvm_demo { opts with parser_kind = Menhir } rest
    | "-parsing" :: _ -> Error "-parsing expects 'topdown' or 'menhir'"
    | "--stop-after" :: "parsing" :: rest ->
      loop llvm_demo { opts with stop_after = Parsing } rest
    | "--stop-after" :: "anf" :: rest ->
      loop llvm_demo { opts with stop_after = Anf_stage } rest
    | "--stop-after" :: "closure" :: rest ->
      loop llvm_demo { opts with stop_after = Closure_stage } rest
    | "--stop-after" :: "lifting" :: rest ->
      loop llvm_demo { opts with stop_after = Lifting_stage } rest
    | "--stop-after" :: "riscv" :: rest ->
      loop llvm_demo { opts with stop_after = Riscv_stage } rest
    | "--stop-after" :: _ ->
      Error "--stop-after expects 'parsing', 'anf', 'closure', 'lifting' or 'riscv'"
    | "-o" :: "-" :: rest -> loop llvm_demo { opts with output = Stdout } rest
    | "-o" :: file :: rest -> loop llvm_demo { opts with output = File file } rest
    | [ "-o" ] -> Error "-o expects a file name or '-'"
    | "--extended" :: rest -> loop llvm_demo { opts with extended = true } rest
    | "--ast" :: rest -> loop llvm_demo { opts with show_ast = true } rest
    | "--llvm-demo" :: rest -> loop true opts rest
    | arg :: _ when String.length arg > 1 && Char.equal arg.[0] '-' ->
      Error (Printf.sprintf "unknown option '%s'" arg)
    | file :: rest ->
      (match opts.input with
       | None -> loop llvm_demo { opts with input = Some file } rest
       | Some _ -> Error "multiple input files specified")
  in
  loop false default_opts args
;;

(* Task-3 LLVM codegen demo, kept behind --llvm-demo so the default
   invocation (task 1: parsing) doesn't require LLVM to be installed. *)
let run_llvm_demo () =
  let context = Llvm.global_context () in
  let builder = Llvm.builder context in
  let () = assert (Llvm_executionengine.initialize ()) in
  let the_module = Llvm.create_module context "main" in
  Llvm.set_target_triple "x86_64-pc-linux-gnu" the_module;
  let _the_execution_engine = Llvm_executionengine.create the_module in
  let module Shmel = (val Shmel.make context builder the_module) in
  let i64_type = Llvm.i64_type context in
  let void_type = Llvm.void_type context in
  let ptr_type = Llvm.pointer_type context in
  let prepare_main print_int =
    let ft =
      let args = Array.make 0 ptr_type in
      Llvm.function_type i64_type args
    in
    let the_function = Llvm.declare_function "main" ft the_module in
    let bb = Llvm.append_block context "entry" the_function in
    Llvm.position_at_end bb builder;
    let (_ : Llvm.llvalue) =
      Shmel.build_call
        (Llvm.function_type void_type [| i64_type |])
        print_int
        [ Llvm.const_int i64_type 70 ]
    in
    let (_ : Llvm.llvalue) = Llvm.build_ret (Llvm.const_int i64_type 0) builder in
    if not (Llvm_analysis.verify_function the_function)
    then (
      Stdlib.Format.printf
        "invalid function generated\n%s\n"
        (Llvm.string_of_llvalue the_function);
      Llvm_analysis.assert_valid_function the_function)
  in
  let _ =
    Llvm.declare_function
      "print_int"
      (Llvm.function_type (Llvm.void_type context) [| i64_type |])
      the_module
  in
  Result.map
    (fun print_int ->
       prepare_main print_int;
       Llvm.print_module "out.ll" the_module)
    (Shmel.lookup_func "print_int")
;;

let parse opts text =
  match opts.parser_kind, opts.extended with
  | Topdown, false -> Main.parse_program_with_comb text
  | Topdown, true -> Main.parse_program_with_comb_extended text
  | Menhir, _ -> Main.parse_program_with_menhir text
;;

let read_input = function
  | None -> Ok (In_channel.input_all stdin)
  | Some file ->
    (try Ok (In_channel.with_open_text file In_channel.input_all) with
     | Sys_error msg -> Error msg)
;;

let generate opts program =
  let print_anf program =
    Ok (fun ppf -> Format.fprintf ppf "%a@." Anf.pp_program program)
  in
  match opts.stop_after with
  | Parsing ->
    Ok
      (fun ppf ->
        if opts.show_ast then Format.fprintf ppf "%a@." Ast.pp_program program;
        Format.fprintf ppf "%s@." (Printast.show_program program))
  | Anf_stage -> print_anf (Anf.of_program program)
  | Closure_stage -> print_anf (Closure_conversion.of_program (Anf.of_program program))
  | Lifting_stage ->
    print_anf
      (Lambda_lifting.of_program (Closure_conversion.of_program (Anf.of_program program)))
  | Riscv_stage ->
    let lifted =
      program
      |> Anf.of_program
      |> Closure_conversion.of_program
      |> Lambda_lifting.of_program
    in
    Result.map
      (fun code ppf -> Format.fprintf ppf "%a@." Shmel_backend.Riscv.pp_instrs code)
      (Shmel_backend.Program.compile lifted)
;;

let write output f =
  match output with
  | Stdout -> Ok (f Format.std_formatter)
  | File file ->
    (try
       Ok
         (Out_channel.with_open_text file (fun oc ->
            f (Format.formatter_of_out_channel oc)))
     with
     | Sys_error msg -> Error msg)
;;

let fail code fmt = Format.kfprintf (fun _ -> exit code) Format.err_formatter fmt

let compile opts =
  match read_input opts.input with
  | Error msg -> fail 1 "Error: %s@." msg
  | Ok text ->
    (match parse opts text with
     | Error (`Parsing_error msg) -> fail 1 "Parser error: %s@." msg
     | Ok program ->
       (match generate opts program with
        | Error msg -> fail 1 "Backend error: %s@." msg
        | Ok output ->
          (match write opts.output output with
           | Ok () -> ()
           | Error msg -> fail 1 "Error: %s@." msg)))
;;

let () =
  let args =
    match Array.to_list Sys.argv with
    | _ :: args -> args
    | [] -> []
  in
  match parse_args args with
  | Error msg -> fail 2 "Error: %s@.Try '--help' for usage.@." msg
  | Ok Help -> print_string usage
  | Ok Llvm_demo ->
    (match run_llvm_demo () with
     | Ok () -> ()
     | Error msg -> fail 1 "Error: %s@." msg)
  | Ok (Compile opts) -> compile opts
;;
