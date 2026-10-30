[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let executable_dir = Filename.dirname (Unix.realpath Sys.executable_name)
let runtime = Filename.concat executable_dir "../tests/runtime_riscv.o"

let value_item name expr =
  Ast.SValue (Ast.NonRec, { Ast.vb_pat = Ast.PVar name; vb_expr = expr }, [])
;;

let division_cases =
  [ 7, 3; -7, 3; 7, -3; -7, -3; 42, 1; 42, -1; -42, -1; 0, 3; 42, 0; 0, 0 ]
;;

let division_structure seed =
  let rand = Random.State.make [| seed |] in
  let numerator, divisor =
    if seed < 0
    then List.nth division_cases ((-seed - 1) / 3 mod List.length division_cases)
    else (
      let numerator = QCheck.Gen.int_range (-1_000_000) 1_000_000 rand in
      let divisor = QCheck.Gen.oneof_list [ 0; 1; -1; 2; -2; 3; -3; 41; -41 ] rand in
      numerator, divisor)
  in
  let var name = Ast.EIdent name in
  let int n = Ast.EConst (Ast.CInt n) in
  let apply fn arg = Ast.EApply (fn, arg) in
  let mode = if seed < 0 then (-seed - 1) mod 3 else seed mod 3 in
  let items =
    if mode = 0
    then
      [ value_item
          "quotient"
          (Ast.EFun (Ast.PVar "x", apply (apply (var "/") (var "x")) (int divisor)))
      ; value_item "qc_result" (apply (var "quotient") (int numerator))
      ]
    else (
      let fn =
        if mode = 1
        then
          Ast.EFun
            ( Ast.PVar "x"
            , Ast.EFun (Ast.PVar "y", apply (apply (var "/") (var "x")) (var "y")) )
        else var "/"
      in
      [ value_item "quotient" fn
      ; value_item "partial" (apply (var "quotient") (int numerator))
      ; value_item "qc_result" (apply (var "partial") (int divisor))
      ])
  in
  items, "qc_result"
;;

let generate kind seed =
  let rand = Random.State.make [| seed |] in
  match kind with
  | "programs" -> QCheck.Gen.generate1 ~rand Typed_gen.arb_observed_structure.gen
  | "closures" -> QCheck.Gen.generate1 ~rand Typed_gen.arb_applied_structure.gen
  | "division" -> division_structure seed
  | _ -> invalid_arg "unknown test kind"
;;

let oracle program result_name =
  match Anf_interp.eval_program program with
  | Ok env ->
    (match Anf_interp.SMap.find_opt result_name env with
     | Some (Value.VInt n) -> Ok (0, string_of_int n, "")
     | _ -> Error "the observation did not produce an integer")
  | Error "division by zero" -> Ok (1, "", "division by zero\n")
  | Error message -> Error message
;;

let write_file path text =
  Out_channel.with_open_bin path (fun out -> output_string out text)
;;

let read_file path = In_channel.with_open_bin path In_channel.input_all

let unwrap = function
  | Ok value -> value
  | Error message -> failwith message
;;

(* Save only failing cases, outside Dune's disposable sandbox. *)
let save_failure kind seed source asm_path =
  let dir =
    match Sys.getenv_opt "DUNE_SOURCEROOT" with
    | Some root -> Filename.concat root "MetaBannedInRfLanguage/_build"
    | None -> executable_dir
  in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
  let base = Filename.concat dir (Printf.sprintf "failed-%s-%d" kind seed) in
  write_file (base ^ ".ml") source;
  write_file
    (base ^ ".s")
    (if Sys.file_exists asm_path
     then read_file asm_path
     else "# Code generation failed before producing assembly.\n");
  write_file (base ^ ".seed") (Printf.sprintf "%s %d\n" kind seed);
  base
;;

let status_to_string = function
  | Unix.WEXITED 124 -> "timeout after 10s"
  | Unix.WEXITED n -> Printf.sprintf "exit %d" n
  | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
  | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n
;;

(* Allow 10 seconds, then terminate; force a kill after one more second if needed. *)
let run base tool args =
  let stdout_path = base ^ ".out" in
  let stderr_path = base ^ ".err" in
  let stdout_fd =
    Unix.openfile stdout_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let pid =
    Fun.protect
      ~finally:(fun () ->
        Unix.close stdout_fd;
        Unix.close stderr_fd)
      (fun () ->
         Unix.create_process
           "timeout"
           (Array.of_list ([ "timeout"; "--kill-after=1s"; "10s"; tool ] @ args))
           Unix.stdin
           stdout_fd
           stderr_fd)
  in
  let _, status = Unix.waitpid [] pid in
  status, read_file stdout_path, read_file stderr_path
;;

let check kind seed =
  let structure, result_name = generate kind seed in
  let converted = Anf_conv.convert structure in
  let expected = Result.bind converted (fun program -> oracle program result_name) in
  (match expected with
   | Error "fuel exhausted (likely non-terminating recursion)" -> QCheck.assume_fail ()
   | _ -> ());
  let output = Ast.SEval (Ast.EApply (Ast.EIdent "print_int", Ast.EIdent result_name)) in
  let source = Pprint.string_of_structure (structure @ [ output ]) in
  let source_path = Filename.temp_file ~temp_dir:executable_dir "qc-" ".ml" in
  let base = Filename.chop_extension source_path in
  let asm_path = base ^ ".s" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix ->
           let path = base ^ suffix in
           if Sys.file_exists path then Sys.remove path)
        [ ".ml"; ".s"; ".exe"; ".out"; ".err" ])
    (fun () ->
       write_file source_path source;
       let result =
         try
           let program = unwrap converted in
           let expected_code, expected_stdout, expected_stderr = unwrap expected in
           let print_result =
             Anf.AEval
               (Anf.ACExpr (Anf.CApp (Anf.ImmVar "print_int", [ Anf.ImmVar result_name ])))
           in
           let program =
             Lambda_lifting.program
               (Closure_conversion.program (program @ [ print_result ]))
           in
           let asm = unwrap (Codegen.gen_program program) in
           write_file asm_path (Riscv.pp_asm asm);
           let exe_path = base ^ ".exe" in
           let status, _, stderr =
             run
               base
               "riscv64-linux-gnu-gcc"
               [ "-march=rv64gc"
               ; "-mabi=lp64d"
               ; "-static"
               ; asm_path
               ; runtime
               ; "-o"
               ; exe_path
               ]
           in
           if status <> Unix.WEXITED 0
           then
             failwith (Printf.sprintf "linker: %s\n%s" (status_to_string status) stderr);
           let status, stdout, stderr =
             run base "qemu-riscv64" [ "-cpu"; "rv64"; exe_path ]
           in
           if
             status = Unix.WEXITED expected_code
             && stdout = expected_stdout
             && stderr = expected_stderr
           then Ok ()
           else
             Error
               (Printf.sprintf
                  "ANF: exit %d, stdout=%S, stderr=%S\nQEMU: %s, stdout=%S, stderr=%S"
                  expected_code
                  expected_stdout
                  expected_stderr
                  (status_to_string status)
                  stdout
                  stderr)
         with
         | exn -> Error (Printexc.to_string exn)
       in
       match result with
       | Ok () -> true
       | Error message ->
         let saved = save_failure kind seed source asm_path in
         QCheck.Test.fail_reportf
           "%s\nkind=%s, seed=%d\nSaved: %s.ml, %s.s, %s.seed"
           message
           kind
           seed
           saved
           saved
           saved)
;;

let () =
  let seeds = QCheck.make ~print:string_of_int (QCheck.Gen.int_bound 0x3fffffff) in
  let tests =
    List.map
      (fun kind ->
         QCheck.Test.make ~name:("ANF vs QEMU: " ^ kind) ~count:100 seeds (check kind))
      [ "programs"; "closures"; "division" ]
  in
  let division =
    QCheck.Test.make
      ~name:"division signs, +/-1, and zero"
      ~count:1
      QCheck.unit
      (fun () ->
         for i = 0 to (3 * List.length division_cases) - 1 do
           ignore (check "division" (-i - 1))
         done;
         true)
  in
  QCheck_runner.run_tests_main (tests @ [ division ])
;;
