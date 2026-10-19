[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let llvm_demo () =
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

type parse_mode =
  | Program
  | Expression

type stage =
  | Parsing
  | Anf

type parser =
  | Topdown
  | Menhir

type diagnostic =
  | Frontend_error of Error_monad.error
  | Anf_error of Middleend.Normalize.error

let parse_program = function
  | Topdown -> Topdown_parser.parse_program
  | Menhir -> Parser_menhir.parse_program
;;

let parse_expression = function
  | Topdown -> Topdown_parser.parse_expression
  | Menhir -> Parser_menhir.parse_expression
;;

let render printer value = Format.asprintf "@[<v>%a@]@." printer value

let compile parser mode stage source =
  let ( let* ) = Result.bind in
  let frontend result = Result.map_error (fun error -> Frontend_error error) result in
  match mode with
  | Expression ->
    let* expression = frontend (parse_expression parser source) in
    Ok (render Ast.pp_expr expression)
  | Program ->
    let* program = frontend (parse_program parser source) in
    (match stage with
     | Parsing -> Ok (render Ast.pp_program program)
     | Anf ->
       let* program =
         Result.map_error
           (fun error -> Anf_error error)
           (Middleend.Normalize.program program)
       in
       Ok (render Middleend.Pretty_printer.pp_program program))
;;

let pp_diagnostic fmt = function
  | Frontend_error error -> Error_monad.pp_error fmt error
  | Anf_error error -> Middleend.Normalize.pp_error fmt error
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
  "compiler.exe [--stop-after parsing|anf] [-parsing topdown|menhir] [-o file|-] [file]\n\
   Defaults: ANF, Menhir, stdout. With options and no file, read stdin.\n\
   With no arguments, generate the legacy LLVM demo in out.ll."
;;

(* Arg callbacks collect settings before validation and processing starts;
   these references belong to this CLI invocation. *)
[@@@zanuda "-mutability_check"]

let () =
  let mode = ref None in
  let stage = ref None in
  let parser = ref Menhir in
  let input_file = ref None in
  let output_file = ref "-" in
  let select_mode selected =
    match !mode with
    | Some current when current <> selected ->
      raise (Arg.Bad "--parse and --parse-expression are mutually exclusive")
    | _ -> mode := Some selected
  in
  let set_input_file path =
    match !input_file with
    | None -> input_file := Some path
    | Some _ -> raise (Arg.Bad "expected at most one input file")
  in
  let options =
    [ ( "--stop-after"
      , Arg.Symbol
          ( [ "parsing"; "anf" ]
          , fun value -> stage := Some (if value = "parsing" then Parsing else Anf) )
      , " Stop after parsing or ANF (default: anf)" )
    ; ( "-parsing"
      , Arg.Symbol
          ( [ "topdown"; "menhir" ]
          , fun value -> parser := if value = "topdown" then Topdown else Menhir )
      , " Choose a parser (default: menhir)" )
    ; ( "-o"
      , Arg.Set_string output_file
      , "FILE Write the result to FILE; '-' means stdout (default: -)" )
    ; "--parse", Arg.Unit (fun () -> select_mode Program), " Print a program AST"
    ; ( "--parse-expression"
      , Arg.Unit (fun () -> select_mode Expression)
      , " Print an expression AST" )
    ]
  in
  try
    Arg.parse_argv Sys.argv options set_input_file usage;
    let selected_stage =
      match !mode, !stage with
      | Some _, Some Anf ->
        raise (Arg.Bad "--parse and --parse-expression conflict with --stop-after anf")
      | Some _, (None | Some Parsing) -> Parsing
      | None, Some stage -> stage
      | None, None -> Anf
    in
    if Array.length Sys.argv = 1
    then llvm_demo ()
    else (
      let source = read_source !input_file in
      match
        compile !parser (Option.value ~default:Program !mode) selected_stage source
      with
      | Ok output -> write_output !output_file output
      | Error error ->
        Format.eprintf "%a@." pp_diagnostic error;
        exit 1)
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
