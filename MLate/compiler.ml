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
    let ( let* ) = Result.bind in
    let* print_int = LL.lookup_func "print_int" in
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
        print_int
        [ Llvm.const_int i64_type 70 ]
    in
    let (_ : Llvm.llvalue) = Llvm.build_ret (Llvm.const_int i64_type 0) builder in
    (* Validate the generated code, checking for consistency. *)
    if Llvm_analysis.verify_function the_function
    then Ok ()
    else Error ("invalid function generated\n" ^ Llvm.string_of_llvalue the_function)
    (* Optimize the function. *)
    (* let (_ : bool) = Llvm.PassManager.run_function the_function the_fpm in *)
    (* Llvm.dump_value the_function; *)
  in
  let _ =
    Llvm.declare_function
      "print_int"
      (Llvm.function_type (Llvm.void_type context) [| i64_type |])
      the_module
  in
  match prepare_main () with
  | Ok () -> Llvm.print_module "out.ll" the_module
  | Error message ->
    prerr_endline message;
    exit 1
;;

module Cli_options = struct
  type parser =
    | Topdown
    | Menhir

  type t =
    { parser : parser
    ; glued_keywords : bool
    }

  type action =
    | Help
    | Parse of t * string

  let usage () =
    print_endline "Usage: compiler [-parsing topdown|menhir] [--glued-keywords] input.ml";
    print_endline "  -parsing topdown|menhir  Select parser (default: topdown)";
    print_endline "  --glued-keywords        Allow keywords inside words (topdown only)";
    print_endline "  -h, --help           Show this help"
  ;;

  let read args =
    let finish options input =
      match options.parser, options.glued_keywords, input with
      | Menhir, true, _ -> Error "--glued-keywords requires -parsing topdown"
      | _, _, None -> Error "missing input file"
      | _, _, Some file -> Ok (Parse (options, file))
    in
    let rec scan options input flags = function
      | [] -> finish options input
      | ("--help" | "-h") :: _ when flags -> Ok Help
      | "--" :: rest when flags -> scan options input false rest
      | "-parsing" :: name :: rest when flags ->
        (match name with
         | "topdown" -> scan { options with parser = Topdown } input flags rest
         | "menhir" -> scan { options with parser = Menhir } input flags rest
         | _ -> Error ("unknown parser: " ^ name ^ " (expected topdown or menhir)"))
      | [ "-parsing" ] when flags -> Error "missing value for -parsing"
      | "--glued-keywords" :: rest when flags ->
        scan { options with glued_keywords = true } input flags rest
      | arg :: _ when flags && String.starts_with ~prefix:"-" arg ->
        Error ("unknown option: " ^ arg)
      | file :: rest ->
        (match input with
         | None -> scan options (Some file) flags rest
         | Some _ -> Error "expected one input file")
    in
    scan { parser = Topdown; glued_keywords = false } None true args
  ;;
end

let frontend args =
  match Cli_options.read args with
  | Error message ->
    prerr_endline message;
    exit 2
  | Ok Help -> Cli_options.usage ()
  | Ok (Parse (options, file)) ->
    (try
       let source = In_channel.with_open_bin file In_channel.input_all in
       let parsed =
         match options.parser with
         | Topdown ->
           MLate_frontend.Topdown.parse_with ~glued_keywords:options.glued_keywords source
         | Menhir -> MLate_frontend.Parse.parse source
       in
       match parsed with
       | Ok program ->
         Printf.printf "Parsed %d top-level bindings\n" (List.length program)
       | Error (`Parsing_error message) ->
         prerr_endline message;
         exit 1
     with
     | Sys_error message ->
       prerr_endline message;
       exit 1)
;;

let () =
  match Array.to_list Sys.argv with
  | [] | [ _ ] -> llvm_demo ()
  | _ :: args -> frontend args
;;
