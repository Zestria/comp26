[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type operator_mode =
  | Standard
  | Extended

type parser =
  | Angstrom of operator_mode
  | Menhir

type command =
  | Run of parser * string option * string option
  | Help of string

(* Arg callbacks return unit. Keep their mutable option state local, then return
   an immutable command. *)
[@@@zanuda "-mutability_check"]

let parse_arguments argv =
  let menhir = ref false in
  let operator_extension = ref false in
  let input = ref None in
  let output = ref None in
  let set_input path =
    match !input with
    | None -> input := Some path
    | Some _ -> raise (Arg.Bad "Expected at most one input file")
  in
  let set_parsing = function
    | "topdown" -> menhir := false
    | "menhir" -> menhir := true
    | name -> raise (Arg.Bad ("Unknown parser: " ^ name ^ "; expected topdown or menhir"))
  in
  let set_stop_after = function
    | "parsing" -> ()
    | name -> raise (Arg.Bad ("Unknown stage: " ^ name ^ "; expected parsing"))
  in
  let options =
    [ "-parsing", Arg.String set_parsing, "NAME Use topdown (default) or menhir"
    ; "--stop-after", Arg.String set_stop_after, "STAGE Stop after parsing"
    ; ( "-o"
      , Arg.String (fun path -> output := Some path)
      , "FILE Write to FILE or - for stdout (default)" )
    ; ( "--operator-extension"
      , Arg.Set operator_extension
      , " Enable operator declarations (Angstrom only)" )
    ; "-", Arg.Unit (fun () -> set_input "-"), " Read standard input"
    ; "--", Arg.Rest set_input, " Treat remaining arguments as filenames"
    ]
  in
  try
    Arg.parse_argv
      ~current:(ref 0)
      argv
      options
      set_input
      "Usage: compiler [-parsing topdown|menhir] [--stop-after parsing] [-o FILE] \
       [--operator-extension] [FILE]\n\
       Without FILE, read standard input.";
    match !menhir, !operator_extension with
    | true, true -> Error "--operator-extension requires the Angstrom parser\n"
    | true, false -> Ok (Run (Menhir, !input, !output))
    | false, true -> Ok (Run (Angstrom Extended, !input, !output))
    | false, false -> Ok (Run (Angstrom Standard, !input, !output))
  with
  | Arg.Help message -> Ok (Help message)
  | Arg.Bad message -> Error message
;;

[@@@zanuda "+mutability_check"]

let io action =
  try Ok (action ()) with
  | Sys_error message -> Error ("I/O error: " ^ message)
;;

let run parser input output =
  let ( let* ) = Result.bind in
  let* source =
    io (fun () ->
      match input with
      | None | Some "-" -> In_channel.input_all stdin
      | Some path -> In_channel.with_open_bin path In_channel.input_all)
  in
  let parse =
    match parser with
    | Angstrom Standard -> Frontend.Parser.parse_program
    | Menhir -> Frontend.Menhir_parser.parse_program
    | Angstrom Extended -> Frontend.Parser.parse_program_with_operators
  in
  let* program =
    Result.map_error (fun message -> "Parse error: " ^ message) (parse source)
  in
  let print channel =
    let formatter = Format.formatter_of_out_channel channel in
    Format.fprintf formatter "%a@." Frontend.Ast_print.program program
  in
  io (fun () ->
    match output with
    | None | Some "-" -> print stdout
    | Some path -> Out_channel.with_open_bin path print)
;;

let main argv =
  match parse_arguments argv with
  | Error message ->
    prerr_string message;
    2
  | Ok (Help message) ->
    print_string message;
    0
  | Ok (Run (parser, input, output)) ->
    (match run parser input output with
     | Ok () -> 0
     | Error message ->
       prerr_endline message;
       1)
;;

let () = exit (main Sys.argv)
