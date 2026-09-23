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
  ; llvm_demo : bool
  }

let default_opts =
  { parser_kind = Comb; extended = false; show_ast = false; llvm_demo = false }
;;

let usage () =
  print_string
    "Usage: compiler.exe [options] [file]\n\
     Reads an expression from <file> (or stdin) and parses it.\n\n\
     Options:\n\
    \  --parser=comb|menhir  Select top-down (default) or bottom-up parser\n\
    \  --extended            Allow reserved words as identifiers (e.g. `let in = 1 in in`); combinator parser only\n\
    \  --ast                 Print the parsed AST\n\
    \  --llvm-demo           Run the LLVM codegen demo instead of parsing (writes out.ll)\n\
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
    | "--llvm-demo" :: rest -> loop { opts with llvm_demo = true } input_file rest
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

(* Task-3 LLVM codegen demo, kept behind --llvm-demo so the default
   invocation (task 1: parsing) doesn't require LLVM to be installed. *)
let run_llvm_demo () =
  let context = Llvm.global_context () in
  let builder = Llvm.builder context in
  let () = assert (Llvm_executionengine.initialize ()) in
  let the_module = Llvm.create_module context "main" in
  Llvm.set_target_triple "x86_64-pc-linux-gnu" the_module;
  let _the_execution_engine = Llvm_executionengine.create the_module in
  let module KormilcynKhmelevYakovlev =
    (val KormilcynKhmelevYakovlev.make context builder the_module)
  in
  let i64_type = Llvm.i64_type context in
  let void_type = Llvm.void_type context in
  let ptr_type = Llvm.pointer_type context in
  let prepare_main () =
    let ft =
      let args = Array.make 0 ptr_type in
      Llvm.function_type i64_type args
    in
    let the_function = Llvm.declare_function "main" ft the_module in
    let bb = Llvm.append_block context "entry" the_function in
    Llvm.position_at_end bb builder;
    let (_ : Llvm.llvalue) =
      KormilcynKhmelevYakovlev.build_call
        (Llvm.function_type void_type [| i64_type |])
        KormilcynKhmelevYakovlev.(lookup_func_exn "print_int")
        [ Llvm.const_int i64_type 70 ]
    in
    let (_ : Llvm.llvalue) = Llvm.build_ret (Llvm.const_int i64_type 0) builder in
    (match Llvm_analysis.verify_function the_function with
     | true -> ()
     | false ->
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
  prepare_main ();
  Llvm.print_module "out.ll" the_module
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
  if opts.llvm_demo
  then run_llvm_demo ()
  else (
    let input_text =
      match input_file with
      | Some filename -> In_channel.(with_open_text filename input_all)
      | None -> In_channel.(input_all stdin)
    in
    run opts input_text)
;;
