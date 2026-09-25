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

let print_result printer = function
  | Ok value -> Format.printf "%a@." printer value
  | Error error ->
    Format.eprintf "%a@." Error_monad.pp_error error;
    exit 1
;;

let parse mode channel =
  let lexbuf = Lexing.from_channel channel in
  match mode with
  | Program -> print_result Ast.pp_program (Parser_menhir.parse_program_lexbuf lexbuf)
  | Expression -> print_result Ast.pp_expr (Parser_menhir.parse_expression_lexbuf lexbuf)
;;

let () =
  let mode = ref None in
  let input_file = ref None in
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
    [ "--parse", Arg.Unit (fun () -> select_mode Program), " Parse a MiniML program"
    ; ( "--parse-expression"
      , Arg.Unit (fun () -> select_mode Expression)
      , " Parse a MiniML expression" )
    ]
  in
  Arg.parse options set_input_file "compiler.exe [--parse | --parse-expression] [file]";
  match !mode, !input_file with
  | None, None -> llvm_demo ()
  | None, Some _ ->
    Format.eprintf "an input file requires --parse or --parse-expression@.";
    exit 2
  | Some mode, input_file ->
    (try
       match input_file with
       | None -> parse mode stdin
       | Some path ->
         let channel = open_in_bin path in
         Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () -> parse mode channel)
     with
     | Sys_error message ->
       Format.eprintf "%s@." message;
       exit 1)
;;
