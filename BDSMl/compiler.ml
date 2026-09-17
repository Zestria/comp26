[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend

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
  | "left" -> Parser_LL.Left
  | "right" -> Parser_LL.Right
  | name -> raise (Arg.Bad ("unknown associativity: " ^ name))
;;

let parse_associativity_override value =
  match String.split_on_char ':' value with
  | [ operator; associativity ] ->
    binop_of_name operator, associativity_of_name associativity
  | _ ->
    raise
      (Arg.Bad
         "expected OPERATOR:left or OPERATOR:right (for example minus:right)")
;;

let print_ll_error = function
  | `lexer_error message | `parse_error message -> message
;;

let run_frontend parser overrides source =
  let result =
    match parser with
    | LR ->
      if overrides <> []
      then Error "--operator-assoc is supported only by the LL parser"
      else Parse.parse source
    | LL ->
      let config =
        List.fold_left
          (fun config (operator, associativity) ->
            Parser_LL.with_associativity config operator associativity)
          Parser_LL.default_config
          (List.rev overrides)
      in
      (match Parser_LL.parse_string_LL ~config source with
       | Ok program -> Ok program
       | Error error -> Error (print_ll_error error))
  in
  match result with
  | Ok program -> print_endline (Printer.print_ast program)
  | Error message ->
    prerr_endline message;
    exit 2
;;

let () =
  let parser = ref LL in
  let source = ref None in
  let overrides = ref [] in
  let options =
    [ ( "--parser"
      , Arg.Symbol
          ([ "ll"; "lr" ], function
            | "ll" -> parser := LL
            | "lr" -> parser := LR
            | _ -> assert false)
      , " Select the parser (default: ll)" )
    ; ( "--operator-assoc"
      , Arg.String
          (fun value -> overrides := parse_associativity_override value :: !overrides)
      , "OPERATOR:left|right Override an LL operator, e.g. minus:right" )
    ; ( "--parse"
      , Arg.String (fun value -> source := Some value)
      , "SOURCE Parse SOURCE and print its AST" )
    ]
  in
  Arg.parse options (fun _ -> raise (Arg.Bad "unexpected positional argument")) "BDSMl";
  match !source with
  | Some source -> run_frontend !parser !overrides source
  | None ->
    if !overrides <> []
    then (
      prerr_endline "--operator-assoc requires --parse";
      exit 2)
    else emit_demo_llvm ()
;;
