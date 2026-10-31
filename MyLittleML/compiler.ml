[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Middle-end driver for MyLittleML (Task 2).

    Reads a program written in an OCaml subset, parses it with one of the
    two parsers (top-down Angstrom parser or bottom-up menhir parser) and
    runs it through the middle-end pipeline: ANF → closure conversion →
    lambda lifting.

    The backend (code generation) is not implemented yet and comes in
    later tasks. *)

type stop_after =
  | Frontend
  | Anf
  | Closure
  | Lambda
  | Riscv

type opts =
  { parser_kind : Mylittleml.Frontend.Parse.parser_kind
  ; sticky : bool
  ; stop_after : stop_after
  ; output : string option
  }

let usage =
  {|Usage: compiler.exe [OPTIONS] [FILE]
Frontend, middle-end and RISC-V backend for MyLittleML.

Options:
  -parsing topdown|menhir    choose the parser (default: topdown;
                             'bottomup' is accepted as an alias of 'menhir')
  --parser topdown|bottomup  legacy alias of -parsing
  --sticky                   enable sticky-keywords mode (top-down only)
  --stop-after parsing|anf|closure-conversion|lambda-lifting|riscv
                             stop the pipeline after the given stage;
                             'riscv' emits RISC-V 64 assembly
                             (default: run the whole pipeline)
  --backend riscv            alias of --stop-after riscv
  -o FILE                    write the result to FILE ('-o -' writes
                             to standard output, which is also the default)
  --ast | --dump-ast         legacy: stop after parsing and print the AST
  --help                     display this help and exit

If FILE is omitted, the program is read from standard input.
|}
;;

let fail_usage msg =
  prerr_endline ("compiler: " ^ msg);
  prerr_endline "Try 'compiler.exe --help' for more information.";
  exit 2
;;

let parse_parser_kind s =
  match s with
  | "topdown" -> Mylittleml.Frontend.Parse.Topdown
  | "menhir" | "bottomup" -> Mylittleml.Frontend.Parse.Bottomup
  | _ -> fail_usage ("unknown parser: " ^ s ^ " (expected 'topdown' or 'menhir')")
;;

let parse_stop_after = function
  | "parsing" -> Frontend
  | "anf" -> Anf
  | "closure-conversion" -> Closure
  | "lambda-lifting" -> Lambda
  | "riscv" -> Riscv
  | s ->
    fail_usage
      ("unknown stage: "
       ^ s
       ^ " (expected 'parsing', 'anf', 'closure-conversion' 'lambda-lifting' or 'riscv')"
      )
;;

let parse_args argv =
  let rec loop ~parser_kind ~sticky ~stop_after ~output ~file = function
    | [] -> file, { parser_kind; sticky; stop_after; output }
    | ("-parsing" | "--parser") :: k :: rest ->
      loop ~parser_kind:(parse_parser_kind k) ~sticky ~stop_after ~output ~file rest
    | ("-parsing" | "--parser") :: [] ->
      fail_usage "option '-parsing' requires an argument ('topdown' or 'menhir')"
    | "--sticky" :: rest -> loop ~parser_kind ~sticky:true ~stop_after ~output ~file rest
    | "--stop-after" :: stage :: rest ->
      loop ~parser_kind ~sticky ~stop_after:(parse_stop_after stage) ~output ~file rest
    | "--stop-after" :: [] -> fail_usage "option '--stop-after' requires an argument"
    | "--backend" :: "riscv" :: rest ->
      loop ~parser_kind ~sticky ~stop_after:Riscv ~output ~file rest
    | "--backend" :: [] -> fail_usage "option '--backend' requires an argument"
    | "--backend" :: b :: _ -> fail_usage ("unknown backend: " ^ b ^ " (expected 'riscv')")
    | "-o" :: path :: rest ->
      loop ~parser_kind ~sticky ~stop_after ~output:(Some path) ~file rest
    | "-o" :: [] -> fail_usage "option '-o' requires an argument"
    | ("--ast" | "--dump-ast") :: rest ->
      loop ~parser_kind ~sticky ~stop_after:Frontend ~output ~file rest
    | "--help" :: _ ->
      print_string usage;
      exit 0
    | a :: rest when String.length a > 0 && a.[0] = '-' ->
      fail_usage ("unrecognized option: " ^ a)
    | f :: rest ->
      (match file with
       | None -> loop ~parser_kind ~sticky ~stop_after ~output ~file:(Some f) rest
       | Some _ -> fail_usage "too many arguments")
  in
  loop
    ~parser_kind:Mylittleml.Frontend.Parse.Topdown
    ~sticky:false
    ~stop_after:Lambda
    ~output:None
    ~file:None
    argv
;;

let read_input = function
  | None -> In_channel.input_all In_channel.stdin
  | Some path ->
    (match In_channel.open_bin path with
     | ch ->
       Fun.protect
         (fun () -> In_channel.input_all ch)
         ~finally:(fun () -> In_channel.close ch)
     | exception Sys_error msg ->
       prerr_endline ("compiler: cannot open file: " ^ msg);
       exit 1)
;;

let run opts source =
  let popts = { Mylittleml.Frontend.Parse.sticky = opts.sticky } in
  let prog =
    match Mylittleml.Frontend.Parse.parse opts.parser_kind popts source with
    | Ok prog -> prog
    | Error msg ->
      prerr_endline ("compiler: " ^ msg);
      exit 1
  in
  let anf_prog = Mylittleml.Middleend.Anf.anf_prog prog in
  let output =
    match opts.stop_after with
    | Frontend -> Mylittleml.Frontend.Pp.to_string prog
    | Anf -> Mylittleml.Middleend.Anf.to_string anf_prog
    | Closure ->
      Mylittleml.Middleend.Anf.to_string
        (Mylittleml.Middleend.Closure.convert_prog anf_prog)
    | Lambda ->
      Mylittleml.Middleend.Anf.to_string
        (Mylittleml.Middleend.Lambda_lift.lift_prog
           (Mylittleml.Middleend.Closure.convert_prog anf_prog))
    | Riscv ->
      let ll_prog =
        Mylittleml.Middleend.Lambda_lift.lift_prog
          (Mylittleml.Middleend.Closure.convert_prog anf_prog)
      in
      (match
         Riscv.Asm.emit
           (Riscv.Codegen.compile_program (Riscv.Codegen.from_aprogram ll_prog))
       with
       | asm -> asm
       | exception Riscv.Codegen.Unsupported msg ->
         prerr_endline ("compiler: backend: " ^ msg);
         exit 1)
  in
  match opts.output with
  | None | Some "-" -> print_endline output
  | Some path ->
    (match Out_channel.open_bin path with
     | ch ->
       Fun.protect
         (fun () -> Out_channel.output_string ch (output ^ "\n"))
         ~finally:(fun () -> Out_channel.close ch)
     | exception Sys_error msg ->
       prerr_endline ("compiler: cannot open output file: " ^ msg);
       exit 1)
;;

let () =
  let argv = List.tl (Array.to_list Sys.argv) in
  let file, opts = parse_args argv in
  let source = read_input file in
  run opts source
;;
