let fail message =
  prerr_endline ("compiler: " ^ message);
  exit 1
;;

let read_file path =
  try Ok (In_channel.with_open_bin path In_channel.input_all) with
  | Sys_error message -> Error message
;;

let write_output (output : Cli.output) text =
  match output with
  | To_stdout ->
    print_string text;
    Ok ()
  | To_file path ->
    (try Ok (Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc text)) with
     | Sys_error message -> Error message)
;;

let build_table = function
  | None -> Ok Syntax.default_table
  | Some settings -> Op_settings.table_of_string settings
;;

let parse_structure (cli : Cli.t) table source =
  match cli.parsing with
  | Topdown -> Rd_parser.parse_structure ~table source
  | Menhir -> Error "the menhir (bottom-up) parser is not implemented yet"
;;

let run (cli : Cli.t) =
  let ( let* ) = Result.bind in
  let* source = read_file cli.input_file in
  let* table = build_table cli.operator_settings in
  let* structure = parse_structure cli table source in
  match cli.stop_after with
  | Stop_after_parsing -> write_output cli.output (Ast.show_structure structure ^ "\n")
  | Stop_after_anf -> Error "--stop-after anf is not implemented yet (ANF pass is not written)"
;;

let () =
  match Cli.parse_arguments Sys.argv with
  | Error message -> fail message
  | Ok Cli.Show_help -> print_string Cli.help_text
  | Ok (Cli.Run cli) ->
    (match run cli with
     | Ok () -> ()
     | Error message -> fail message)
;;
