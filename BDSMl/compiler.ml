[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend
open BDSMl_middleend

type parser_kind =
  | LL
  | LR

let emit_demo_llvm () =
  let context = Llvm.global_context () in
  let builder = Llvm.builder context in
  let () = assert (Llvm_executionengine.initialize ()) in
  let the_module = Llvm.create_module context "main" in
  Llvm.set_target_triple "x86_64-pc-linux-gnu" the_module;
  let _the_execution_engine = Llvm_executionengine.create the_module in
  let module LL = (val LL.make context builder the_module) in
  let i64_type = Llvm.i64_type context in
  let void_type = Llvm.void_type context in
  let ptr_type = Llvm.pointer_type context in
  let prepare_main () =
    let ft =
      (* TODO main has special args *)
      let args = Array.make 0 ptr_type in
      Llvm.function_type i64_type args
    in
    let the_function = Llvm.declare_function "main" ft the_module in
    (* Create a new basic block to start insertion into. *)
    let bb = Llvm.append_block context "entry" the_function in
    Llvm.position_at_end bb builder;
    (* Add all arguments to the symbol table and create their allocas. *)
    (* Finish off the function. *)
    let (_ : Llvm.llvalue) =
      LL.build_call
        (Llvm.function_type void_type [| i64_type |])
        LL.(lookup_func_exn "print_int")
        [ Llvm.const_int i64_type 70 ]
    in
    let (_ : Llvm.llvalue) = Llvm.build_ret (Llvm.const_int i64_type 0) builder in
    (* Validate the generated code, checking for consistency. *)
    (match Llvm_analysis.verify_function the_function with
     | true -> ()
     | false ->
       Stdlib.Format.printf
         "invalid function generated\n%s\n"
         (Llvm.string_of_llvalue the_function);
       Llvm_analysis.assert_valid_function the_function);
    (* Optimize the function. *)
    (* let (_ : bool) = Llvm.PassManager.run_function the_function the_fpm in *)
    (* Llvm.dump_value the_function; *)
    ()
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

let binop_of_name = function
  | "plus" -> Ast.Plus
  | "minus" -> Ast.Minus
  | "mul" -> Ast.Mult
  | "div" -> Ast.Div
  | "equal" -> Ast.Equal
  | "not-equal" -> Ast.NotEqual
  | "greater" -> Ast.More
  | "less" -> Ast.Less
  | "greater-equal" -> Ast.EMore
  | "less-equal" -> Ast.ELess
  | "and" -> Ast.And
  | "or" -> Ast.Or
  | name -> raise (Arg.Bad ("unknown operator: " ^ name))
;;

let associativity_of_name = function
  | "left" -> LL_parser.Parser.Left
  | "right" -> LL_parser.Parser.Right
  | name -> raise (Arg.Bad ("unknown associativity: " ^ name))
;;

let parse_associativity_override value =
  match String.split_on_char ':' value with
  | [ operator; associativity ] ->
    binop_of_name operator, associativity_of_name associativity
  | _ ->
    raise (Arg.Bad "expected OPERATOR:left or OPERATOR:right (for example minus:right)")
;;

let print_ll_error = function
  | `lexer_error message | `parse_error message -> message
;;

let parse_source parser overrides source =
  match parser with
  | LR ->
    if overrides <> []
    then Error "--operator-assoc is supported only by the topdown parser"
    else Parse.parse source
  | LL ->
    let config =
      List.fold_left
        (fun config (operator, associativity) ->
           LL_parser.Parser.with_associativity config operator associativity)
        LL_parser.Parser.default_config
        (List.rev overrides)
    in
    (match LL_parser.Parser.parse_string_LL ~config source with
     | Ok program -> Ok program
     | Error error -> Error (print_ll_error error))
;;

let parse_or_exit parser overrides source =
  match parse_source parser overrides source with
  | Ok program -> program
  | Error message ->
    prerr_endline message;
    exit 2
;;

let read_file filename =
  try
    let channel = open_in_bin filename in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  with
  | Sys_error message ->
    prerr_endline message;
    exit 2
;;

let write_output filename text =
  match filename with
  | "-" -> print_endline text
  | filename ->
    (try
       let channel = open_out_bin filename in
       Fun.protect
         ~finally:(fun () -> close_out_noerr channel)
         (fun () ->
           output_string channel text;
           output_char channel '\n')
     with
     | Sys_error message ->
       prerr_endline message;
       exit 2)
;;

let stage_of_name = function
  | "parsing" -> Pipeline.Parsing
  | "alpha" -> Pipeline.Alpha
  | "cc" -> Pipeline.Closure_conv
  | "ll" -> Pipeline.Lambda_lift
  | "anf" -> Pipeline.Anf
  | name -> raise (Arg.Bad ("unknown stage: " ^ name))
;;

let () =
  let parser = ref LL in
  let inline_source = ref None in
  let input_file = ref None in
  let output_file = ref "-" in
  let stop_after = ref Pipeline.Anf in
  let pipeline_mode = ref false in
  let overrides = ref [] in
  let options =
    [ ( "--parser"
      , Arg.Symbol
          ( [ "ll"; "lr" ]
          , function
            | "ll" -> parser := LL
            | "lr" -> parser := LR
            | _ -> assert false )
      , " Select the parser (default: ll)" )
    ; ( "-parsing"
      , Arg.Symbol
          ( [ "topdown"; "menhir" ]
          , function
            | "topdown" ->
              pipeline_mode := true;
              parser := LL
            | "menhir" ->
              pipeline_mode := true;
              parser := LR
            | _ -> assert false )
      , " Select the parser (default: topdown)" )
    ; ( "--stop-after"
      , Arg.Symbol
          ( [ "parsing"; "alpha"; "cc"; "ll"; "anf" ]
          , fun stage ->
              pipeline_mode := true;
              stop_after := stage_of_name stage )
      , " Stop after this compilation stage" )
    ; ( "-o"
      , Arg.String (fun filename ->
          pipeline_mode := true;
          output_file := filename)
      , "FILE Write output to FILE; use - for stdout" )
    ; ( "--operator-assoc"
      , Arg.String
          (fun value -> overrides := parse_associativity_override value :: !overrides)
      , "OPERATOR:left|right Override an LL operator, e.g. minus:right" )
    ; ( "--parse"
      , Arg.String (fun value -> inline_source := Some value)
      , "SOURCE Parse SOURCE and print its AST" )
    ]
  in
  let set_input filename =
    match !input_file with
    | None -> input_file := Some filename
    | Some _ -> raise (Arg.Bad "only one input file may be specified")
  in
  Arg.parse options set_input "Usage: BDSMl [OPTIONS] [FILE.ml]";
  match !inline_source, !input_file with
  | Some _, Some _ ->
    prerr_endline "--parse and an input file cannot be used together";
    exit 2
  | Some source, None ->
    let program = parse_or_exit !parser !overrides source in
    write_output !output_file (Printer.print_ast program)
  | None, Some filename ->
    let source = read_file filename in
    let program = parse_or_exit !parser !overrides source in
    write_output !output_file (Pipeline.run ~stop_after:!stop_after program)
  | None, None ->
    if !overrides <> []
    then (
      prerr_endline "--operator-assoc requires --parse or an input file";
      exit 2)
    else if !pipeline_mode
    then (
      prerr_endline "an input .ml file is required";
      exit 2)
    else emit_demo_llvm ()
;;
