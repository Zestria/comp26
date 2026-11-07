[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_middleend

let seed = 2026
let case_count = 96
let batch_size = 16
let subprocess_timeout = "5"
let maximum_trivial_cases = 8
let gcc = "riscv64-linux-gnu-gcc"
let qemu = "qemu-riscv64"

type parser =
  { name : string
  ; parse : string -> (Bkml_frontend.Ast.program, string) result
  }

type case =
  { index : int
  ; source : string
  ; expected : string
  }

type outcome =
  { stdout : string
  ; stderr : string
  ; exit_code : int
  ; command : string
  }

type execution =
  { assembly : string
  ; outcome : outcome
  }

type comparison_error =
  | Invalid_output of int * string
  | Different_value of int * string

type assembly_metrics =
  { call_ml : int
  ; jalr : int
  ; rt_apply : int
  ; rt_alloc : int
  ; mul : int
  ; div_rem : int
  ; branches : int
  }

let empty_metrics =
  { call_ml = 0
  ; jalr = 0
  ; rt_apply = 0
  ; rt_alloc = 0
  ; mul = 0
  ; div_rem = 0
  ; branches = 0
  }
;;

let add_metrics a b =
  { call_ml = a.call_ml + b.call_ml
  ; jalr = a.jalr + b.jalr
  ; rt_apply = a.rt_apply + b.rt_apply
  ; rt_alloc = a.rt_alloc + b.rt_alloc
  ; mul = a.mul + b.mul
  ; div_rem = a.div_rem + b.div_rem
  ; branches = a.branches + b.branches
  }
;;

let encode values = String.concat "" (List.map (fun value -> value ^ "-1") values)

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let decode output =
  let length = String.length output in
  let rec records offset index values =
    if offset = length
    then Ok (List.rev values)
    else (
      let sign_end = if output.[offset] = '-' then offset + 1 else offset in
      let rec digits end_ =
        if end_ < length && is_digit output.[end_] then digits (end_ + 1) else end_
      in
      let value_end = digits sign_end in
      if value_end = sign_end
      then
        Error (index, Printf.sprintf "expected decimal digits at output offset %d" offset)
      else if
        value_end + 1 >= length
        || output.[value_end] <> '-'
        || output.[value_end + 1] <> '1'
      then
        Error
          ( index
          , Printf.sprintf "expected -1 record terminator at output offset %d" value_end
          )
      else (
        let value = String.sub output offset (value_end - offset) in
        records (value_end + 2) (index + 1) (value :: values)))
  in
  records 0 0 []
;;

let compare_decoded expected output =
  match decode output with
  | Error (index, message) -> Error (Invalid_output (index, message))
  | Ok actual ->
    let rec compare index expected actual =
      match expected, actual with
      | [], [] -> Ok ()
      | [], _ | _, [] ->
        Error (Invalid_output (index, "decoded record count differs from the case count"))
      | expected :: rest, actual :: actual_rest when expected = actual ->
        compare (index + 1) rest actual_rest
      | _ :: _, actual :: _ ->
        Error
          (Different_value
             (index, Printf.sprintf "decoded value %S differs from the host oracle" actual))
    in
    compare 0 expected actual
;;

let self_check () =
  let check count action =
    action ();
    count + 1
  in
  let count =
    List.fold_left
      (fun count (values, wire) ->
         let count =
           check count (fun () ->
             if encode values <> wire then failwith "record encoder self-check failed")
         in
         check count (fun () ->
           match decode wire with
           | Ok actual when actual = values -> ()
           | Ok _ | Error _ -> failwith "record decoder round-trip self-check failed"))
      0
      [ [], ""
      ; [ "1"; "23" ], "1-123-1"
      ; [ "12"; "3" ], "12-13-1"
      ; [ "-1"; "1" ], "-1-11-1"
      ; [ "-1"; "-1" ], "-1-1-1-1"
      ]
  in
  let count =
    List.fold_left
      (fun count invalid ->
         check count (fun () ->
           match decode invalid with
           | Error _ -> ()
           | Ok _ -> failwith ("record decoder accepted malformed output " ^ invalid)))
      count
      [ "1"; "1-"; "1-10"; "1-1x"; "--1-1" ]
  in
  let count =
    check count (fun () ->
      if String.concat "" [ "1"; "23" ] <> String.concat "" [ "12"; "3" ]
      then failwith "raw-concatenation collision witness is invalid")
  in
  let count =
    check count (fun () ->
      if encode [ "1"; "23" ] = encode [ "12"; "3" ]
      then failwith "record encoding has a raw-concatenation collision")
  in
  let count =
    check count (fun () ->
      match compare_decoded [ "1"; "23" ] (encode [ "12"; "3" ]) with
      | Error (Different_value _) -> ()
      | Ok () | Error (Invalid_output _) ->
        failwith "record decoder missed the raw-concatenation collision witness")
  in
  Printf.printf "decoder_self_checks=%d\n%!" count
;;

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path text =
  Out_channel.with_open_bin path (fun out -> output_string out text)
;;

let run_process directory tag program arguments =
  let stdout_path = Filename.concat directory (tag ^ ".stdout") in
  let stderr_path = Filename.concat directory (tag ^ ".stderr") in
  let arguments = [ "--kill-after=1s"; subprocess_timeout; program ] @ arguments in
  let command = String.concat " " ("timeout" :: arguments) in
  let flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
  let stdout = Unix.openfile stdout_path flags 0o600 in
  let stderr = Unix.openfile stderr_path flags 0o600 in
  let pid =
    match
      Unix.create_process
        "timeout"
        (Array.of_list ("timeout" :: arguments))
        Unix.stdin
        stdout
        stderr
    with
    | pid -> pid
    | exception exn ->
      Unix.close stdout;
      Unix.close stderr;
      raise exn
  in
  Unix.close stdout;
  Unix.close stderr;
  let _, status = Unix.waitpid [] pid in
  let exit_code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal
  in
  { stdout = read_file stdout_path; stderr = read_file stderr_path; exit_code; command }
;;

let run_required directory tag program arguments =
  match run_process directory tag program arguments with
  | outcome when outcome.exit_code = 0 -> outcome
  | outcome ->
    Printf.eprintf
      "required command failed: %s\nexit=%d\nstdout=%S\nstderr=%S\n%!"
      outcome.command
      outcome.exit_code
      outcome.stdout
      outcome.stderr;
    exit 1
  | exception exn ->
    Printf.eprintf
      "required command could not be started: timeout --kill-after=1s %s %s: %s\n%!"
      subprocess_timeout
      (String.concat " " (program :: arguments))
      (Printexc.to_string exn);
    exit 1
;;

let fail_observation ~index ~parser ~source ~expected ~actual ~detail =
  Printf.eprintf
    "test_backend_random failed\n\
     seed=%d\n\
     case_index=%d\n\
     parser=%s\n\
     source:\n\
     %s\n\
     expected_stdout=%S\n\
     expected_exit=0\n\
     actual_stdout=%S\n\
     actual_exit=%d\n\
     actual_stderr=%S\n\
     actual_command=%s\n\
     detail=%s\n\
     %!"
    seed
    index
    parser.name
    source
    expected
    actual.stdout
    actual.exit_code
    actual.stderr
    actual.command
    detail;
  exit 1
;;

let compile_error_outcome detail =
  { stdout = ""; stderr = detail; exit_code = -1; command = "" }
;;

let compile_lifted parser source =
  try
    let ( let* ) = Result.bind in
    let* ast =
      Result.map_error (fun error -> "parse error: " ^ error) (parser.parse source)
    in
    Result.map_error
      (fun error -> "middleend error: " ^ Bkml_middleend.Pipeline.error_to_string error)
      (Bkml_middleend.Pipeline.compile ast)
  with
  | exn -> Error ("unexpected parse/compile exception: " ^ Printexc.to_string exn)
;;

let compile_source parser source =
  try
    match compile_lifted parser source with
    | Error error -> Error error
    | Ok lifted ->
      Result.map_error
        (fun error -> "codegen error: " ^ Bkml_backend.Codegen.error_to_string error)
        (Bkml_backend.Codegen.compile lifted)
  with
  | exn -> Error ("unexpected codegen exception: " ^ Printexc.to_string exn)
;;

let execute directory runtime_object parser stem source =
  try
    match compile_source parser source with
    | Error error -> Error error
    | Ok assembly ->
      let assembly_path = Filename.concat directory (stem ^ ".s") in
      let executable_path = Filename.concat directory (stem ^ ".bin") in
      write_file assembly_path assembly;
      let linked =
        run_process
          directory
          (stem ^ ".link")
          gcc
          [ "-static"; assembly_path; runtime_object; "-o"; executable_path ]
      in
      if linked.exit_code <> 0
      then
        Error
          (Printf.sprintf
             "linker command=%s; exit=%d; stdout=%S; stderr=%S"
             linked.command
             linked.exit_code
             linked.stdout
             linked.stderr)
      else (
        let outcome = run_process directory (stem ^ ".run") qemu [ executable_path ] in
        Ok { assembly; outcome })
  with
  | exn -> Error ("unexpected process/I/O exception: " ^ Printexc.to_string exn)
;;

let fail_case parser case actual detail =
  fail_observation
    ~index:case.index
    ~parser
    ~source:case.source
    ~expected:(encode [ case.expected ])
    ~actual
    ~detail
;;

let rec check_individually directory runtime_object parser = function
  | [] -> ()
  | case :: rest ->
    let stem = Printf.sprintf "case-%s-%04d" parser.name case.index in
    (match execute directory runtime_object parser stem case.source with
     | Error error ->
       fail_case
         parser
         case
         (compile_error_outcome error)
         "individual compilation, linking, or process creation failed after a batch \
          mismatch"
     | Ok { outcome; _ } ->
       (match compare_decoded [ case.expected ] outcome.stdout with
        | Ok () when outcome.exit_code = 0 ->
          check_individually directory runtime_object parser rest
        | comparison ->
          let detail =
            if outcome.exit_code = 124
            then "subprocess timed out (exit 124)"
            else if outcome.exit_code <> 0
            then Printf.sprintf "individual process exited %d" outcome.exit_code
            else (
              match comparison with
              | Error (Invalid_output (_, message) | Different_value (_, message)) ->
                message
              | Ok () -> "individual result differs after a batch mismatch")
          in
          fail_case parser case outcome detail))
;;

let batch_sources batch =
  String.concat
    "\n"
    (List.map
       (fun case -> Printf.sprintf "case_index=%d source=%s" case.index case.source)
       batch)
;;

let fail_batch parser batch source expected actual relative_index detail =
  let first =
    match batch with
    | first :: _ -> first
    | [] -> assert false
  in
  fail_observation
    ~index:(first.index + relative_index)
    ~parser
    ~source
    ~expected:(encode expected)
    ~actual
    ~detail:(detail ^ "\nper-case sources:\n" ^ batch_sources batch)
;;

let contains_substring text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec at offset =
    offset + fragment_length <= text_length
    && (String.sub text offset fragment_length = fragment || at (offset + 1))
  in
  fragment_length = 0 || at 0
;;

let has_instruction line instruction =
  let line = String.trim line in
  String.starts_with ~prefix:(instruction ^ "\t") line
  || String.starts_with ~prefix:(instruction ^ " ") line
;;

let measure_assembly assembly =
  let increment condition = if condition then 1 else 0 in
  String.split_on_char '\n' assembly
  |> List.fold_left
       (fun metrics line ->
          let call = has_instruction line "call" in
          let branch =
            List.exists
              (fun instruction -> has_instruction line instruction)
              [ "beqz"; "bnez"; "beq"; "bne"; "blt"; "bge"; "bltu"; "bgeu" ]
          in
          { call_ml = metrics.call_ml + increment (call && contains_substring line "ml_")
          ; jalr = metrics.jalr + increment (has_instruction line "jalr")
          ; rt_apply = metrics.rt_apply + increment (contains_substring line "rt_apply")
          ; rt_alloc = metrics.rt_alloc + increment (contains_substring line "rt_alloc")
          ; mul = metrics.mul + increment (has_instruction line "mul")
          ; div_rem =
              metrics.div_rem
              + increment (has_instruction line "div" || has_instruction line "rem")
          ; branches = metrics.branches + increment branch
          })
       empty_metrics
;;

let print_metrics parser batch metrics =
  let first =
    match batch with
    | first :: _ -> first
    | [] -> assert false
  in
  Printf.printf
    "assembly parser=%s batch=%d call_ml=%d jalr=%d rt_apply=%d rt_alloc=%d mul=%d \
     div_rem=%d branches=%d\n\
     %!"
    parser.name
    first.index
    metrics.call_ml
    metrics.jalr
    metrics.rt_apply
    metrics.rt_alloc
    metrics.mul
    metrics.div_rem
    metrics.branches
;;

let check_batch directory runtime_object parser batch =
  match batch with
  | [] -> empty_metrics
  | first :: _ ->
    let source = String.concat ";;\n" (List.map (fun case -> case.source) batch) in
    let expected = List.map (fun case -> case.expected) batch in
    let stem = Printf.sprintf "batch-%s-%04d" parser.name first.index in
    let result = execute directory runtime_object parser stem source in
    let metrics assembly =
      let metrics = measure_assembly assembly in
      print_metrics parser batch metrics;
      metrics
    in
    (match result with
     | Ok { assembly; outcome } when outcome.exit_code = 0 ->
       (match compare_decoded expected outcome.stdout with
        | Ok () -> metrics assembly
        | Error (Invalid_output (relative_index, message)) ->
          fail_batch parser batch source expected outcome relative_index message
        | Error (Different_value (relative_index, message)) ->
          check_individually directory runtime_object parser batch;
          fail_batch
            parser
            batch
            source
            expected
            outcome
            relative_index
            (message ^ "; every individual case passed"))
     | Ok { outcome; _ } ->
       check_individually directory runtime_object parser batch;
       fail_batch
         parser
         batch
         source
         expected
         outcome
         0
         "batch process failed, but every individual case passed"
     | Error error ->
       check_individually directory runtime_object parser batch;
       fail_batch
         parser
         batch
         source
         expected
         (compile_error_outcome error)
         0
         "batch compilation failed, but every individual case passed")
;;

let generate_cases () =
  let random = Random.State.make [| seed |] in
  List.init case_count (fun index ->
    match QCheck.Gen.generate ~rand:random ~n:1 Typed_terms.arbitrary_int.gen with
    | [ term ] ->
      { index
      ; source =
          Printf.sprintf "print_int (%s);; print_int (-1)" (Typed_terms.source term)
      ; expected = Typed_terms.expected term
      }
    | _ -> failwith "QCheck returned an unexpected generated case count")
;;

let is_constant_print computation =
  match computation with
  | Anf.Apply
      ( Lifted.Indirect (Anf.Primitive Bkml_middleend.Primitive.Print_int)
      , Anf.Constant _
      , [] ) -> true
  | Atom _ | Tuple _ | Field _ | Function _ | Apply _ | If _ -> false
;;

let rec only_constant_prints = function
  | Anf.Return (Anf.Atom (Anf.Constant _)) -> true
  | Anf.Let (_, computation, rest) | Anf.Sequence (computation, rest) ->
    is_constant_print computation && only_constant_prints rest
  | Anf.Return _ | Anf.Let_rec _ -> false
;;

let trivial_after_pipeline (program : Lifted.program) =
  program.functions = [] && only_constant_prints program.body
;;

let count_trivial_cases parser cases =
  List.fold_left
    (fun count case ->
       match compile_lifted parser case.source with
       | Error error ->
         fail_case
           parser
           case
           (compile_error_outcome error)
           "post-Pipeline degeneracy analysis failed"
       | Ok lifted -> if trivial_after_pipeline lifted then count + 1 else count)
    0
    cases
;;

let rec take count values =
  match count, values with
  | 0, _ | _, [] -> [], values
  | count, value :: rest ->
    let taken, rest = take (count - 1) rest in
    value :: taken, rest
;;

let rec batches = function
  | [] -> []
  | cases ->
    let batch, rest = take batch_size cases in
    batch :: batches rest
;;

(* Failure reports call [exit], which skips [Fun.protect] finalizers. Retaining
   those temporary files aids diagnosis; successful runs remove them here. *)
let with_temp_dir action =
  let directory = Filename.temp_dir "backend-random" "" in
  Fun.protect
    ~finally:(fun () ->
      Sys.readdir directory
      |> Array.iter (fun name -> Unix.unlink (Filename.concat directory name));
      Unix.rmdir directory)
    (fun () -> action directory)
;;

let runtime_path () =
  let executable_directory = Unix.realpath Sys.executable_name |> Filename.dirname in
  Filename.concat executable_directory "../runtime/runtime.c"
;;

let require_machine_paths metrics =
  let missing =
    List.filter_map
      (fun (name, actual, minimum) ->
         if actual < minimum
         then Some (Printf.sprintf "%s=%d (minimum %d)" name actual minimum)
         else None)
      [ "call_ml", metrics.call_ml, case_count * 2
      ; "jalr", metrics.jalr, case_count / 2
      ; "rt_apply", metrics.rt_apply, case_count / 2
      ; "rt_alloc", metrics.rt_alloc, case_count
      ; "branches", metrics.branches, case_count / 2
      ]
  in
  if missing <> []
  then (
    Printf.eprintf
      "generated assembly lacks required machine paths: %s\n%!"
      (String.concat ", " missing);
    exit 1)
;;

let () =
  let started = Unix.gettimeofday () in
  self_check ();
  if case_count <= 0 || batch_size <= 0
  then failwith "backend generation must be non-empty";
  let cases = generate_cases () in
  if cases = [] then failwith "backend generator produced zero cases";
  let parsers =
    [ { name = "topdown"; parse = Bkml_frontend.Parser.parse_program }
    ; { name = "menhir"; parse = Bkml_frontend.Menhir_parser.parse_program }
    ]
  in
  let analysis_parser =
    match parsers with
    | parser :: _ -> parser
    | [] -> assert false
  in
  let trivial_cases = count_trivial_cases analysis_parser cases in
  Printf.printf "post_pipeline_trivial_constants=%d/%d\n%!" trivial_cases case_count;
  if trivial_cases > maximum_trivial_cases
  then (
    Printf.eprintf
      "too many trivial constant programs after Pipeline: %d/%d\n%!"
      trivial_cases
      case_count;
    exit 1);
  let totals =
    with_temp_dir (fun directory ->
      ignore (run_required directory "gcc-version" gcc [ "--version" ]);
      ignore (run_required directory "qemu-version" qemu [ "--version" ]);
      let runtime = runtime_path () in
      if not (Sys.file_exists runtime) then failwith ("runtime.c not found at " ^ runtime);
      let runtime_object = Filename.concat directory "runtime.o" in
      ignore
        (run_required
           directory
           "runtime.compile"
           gcc
           [ "-c"; runtime; "-o"; runtime_object ]);
      List.fold_left
        (fun totals parser ->
           List.fold_left
             (fun totals batch ->
                add_metrics totals (check_batch directory runtime_object parser batch))
             totals
             (batches cases))
        empty_metrics
        parsers)
  in
  print_metrics
    { name = "total"; parse = Bkml_frontend.Parser.parse_program }
    cases
    totals;
  require_machine_paths totals;
  Printf.printf
    "test_backend_random: seed=%d cases=%d batch_size=%d elapsed=%.3fs\n%!"
    seed
    case_count
    batch_size
    (Unix.gettimeofday () -. started)
;;
