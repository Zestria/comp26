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
    let print_int_function =
      match LL.lookup_func "print_int" with
      | Ok function_ -> function_
      | Error message ->
        prerr_endline message;
        exit 2
    in
    let (_ : Llvm.llvalue) =
      LL.build_call
        (Llvm.function_type void_type [| i64_type |])
        print_int_function
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

(** Immutable command-line configuration. *)
type options =
  { parser : parser_kind (** Selected frontend. *)
  ; inline_source : string option (** Source supplied with [--parse]. *)
  ; input_file : string option (** Positional input filename. *)
  ; output_file : string (** Destination, or [-] for stdout. *)
  ; stop_after : Pipeline.stage (** Last compilation stage. *)
  ; pipeline_mode : bool (** Whether an input file is required. *)
  ; overrides : (Ast.binop * LL_parser.Parser.associativity) list
      (** Associativity overrides in reverse command-line order. *)
  ; show_help : bool (** Print usage and exit. *)
  }

let usage =
  "Usage: BDSMl [OPTIONS] [FILE.ml]\n\
   --parser ll|lr                 Select parser (default: ll)\n\
   -parsing topdown|menhir         Select parser\n\
   --stop-after parsing|alpha|cc|ll|anf\n\
   -o FILE                        Write output; - means stdout\n\
   --operator-assoc OP:left|right  Override LL operator associativity\n\
   --parse SOURCE                 Parse inline source and print AST\n\
   -help, --help                  Show this help"
;;

let default_options =
  { parser = LL
  ; inline_source = None
  ; input_file = None
  ; output_file = "-"
  ; stop_after = Pipeline.Anf
  ; pipeline_mode = false
  ; overrides = []
  ; show_help = false
  }
;;

let add_input options filename =
  match options.input_file with
  | None -> Ok { options with input_file = Some filename }
  | Some _ -> Error "only one input file may be specified"
;;

let parse_options arguments =
  let ( let* ) = Result.bind in
  let rec positional options = function
    | [] -> Ok options
    | filename :: rest ->
      let* options = add_input options filename in
      positional options rest
  in
  let rec parse options = function
    | [] -> Ok options
    | ("-help" | "--help") :: _ -> Ok { options with show_help = true }
    | "--" :: rest -> positional options rest
    | ("--parser" | "-parsing" | "--stop-after" | "-o" | "--operator-assoc" | "--parse" as flag)
      :: rest ->
      (match rest with
       | [] -> Error (flag ^ " requires an argument")
       | value :: rest ->
         let* options =
           match flag, value with
           | "--parser", "ll" -> Ok { options with parser = LL }
           | "--parser", "lr" -> Ok { options with parser = LR }
           | "-parsing", "topdown" ->
             Ok { options with parser = LL; pipeline_mode = true }
           | "-parsing", "menhir" ->
             Ok { options with parser = LR; pipeline_mode = true }
           | ("--parser" | "-parsing"), _ -> Error ("unknown parser: " ^ value)
           | "--stop-after", _ ->
             Ok { options with stop_after = stage_of_name value; pipeline_mode = true }
           | "-o", _ -> Ok { options with output_file = value; pipeline_mode = true }
           | "--operator-assoc", _ ->
             Ok { options with overrides = parse_associativity_override value :: options.overrides }
           | "--parse", _ -> Ok { options with inline_source = Some value }
           | _ -> Error ("unknown option: " ^ flag)
         in
         parse options rest)
    | argument :: rest when String.length argument > 1 && argument.[0] = '-' ->
      (match String.index_opt argument '=' with
       | Some index ->
         let flag = String.sub argument 0 index in
         let value = String.sub argument (index + 1) (String.length argument - index - 1) in
         parse options (flag :: value :: rest)
       | None -> Error ("unknown option: " ^ argument))
    | filename :: rest ->
      let* options = add_input options filename in
      parse options rest
  in
  try parse default_options arguments with
  | Arg.Bad message -> Error message
;;

let () =
  let options =
    match parse_options (List.tl (Array.to_list Sys.argv)) with
    | Ok options -> options
    | Error message ->
      prerr_endline message;
      prerr_endline usage;
      exit 2
  in
  if options.show_help
  then print_endline usage
  else
    match options.inline_source, options.input_file with
    | Some _, Some _ ->
      prerr_endline "--parse and an input file cannot be used together";
      exit 2
    | Some source, None ->
      let program = parse_or_exit options.parser options.overrides source in
      write_output options.output_file (Printer.print_ast program)
    | None, Some filename ->
      let source = read_file filename in
      let program = parse_or_exit options.parser options.overrides source in
      (match Pipeline.run ~stop_after:options.stop_after program with
       | Ok output -> write_output options.output_file output
       | Error message ->
         prerr_endline message;
         exit 2)
    | None, None ->
      if options.overrides <> []
      then (
        prerr_endline "--operator-assoc requires --parse or an input file";
        exit 2)
      else if options.pipeline_mode
      then (
        prerr_endline "an input .ml file is required";
        exit 2)
      else emit_demo_llvm ()
;;
