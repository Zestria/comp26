open BDSMl_frontend
open BDSMl_frontend.Ast
open BDSMl_middleend
open BDSMl_middleend.Anf.Anf_ast
open BDSMl_backend

let unwrap = function Ok value -> value | Error error -> Alcotest.fail error

let lower source =
  let ast = Parse.parse source |> unwrap in
  Pipeline.to_anf ast |> unwrap
;;

let generate source = Codegen.generate (lower source) |> unwrap

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))
;;

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)
;;

let command executable arguments stdout_path stderr_path =
  let stdout = Unix.openfile stdout_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let stderr = Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let pid =
    Fun.protect ~finally:(fun () -> Unix.close stdout; Unix.close stderr)
      (fun () -> Unix.create_process executable (Array.of_list (executable :: arguments)) Unix.stdin stdout stderr)
  in
  snd (Unix.waitpid [] pid)
;;

let execute_assembly ?(status = 0) ?(stderr = "") expected assembly =
  let assembly_path = Filename.temp_file "bdsml-backend-" ".s" in
  let executable_path = Filename.temp_file "bdsml-backend-" ".exe" in
  let stdout_path = Filename.temp_file "bdsml-backend-" ".stdout" in
  let stderr_path = Filename.temp_file "bdsml-backend-" ".stderr" in
  Fun.protect
    ~finally:(fun () -> List.iter Sys.remove [ assembly_path; executable_path; stdout_path; stderr_path ])
    (fun () ->
      write_file assembly_path assembly;
      let compilation =
        command "riscv64-linux-gnu-gcc"
          [ "-march=rv64gc"; "-mabi=lp64d"; "-std=c11"; "-Wall"; "-Wextra"; "-Werror"
          ; "-static"; "-no-pie"; assembly_path
          ; "../runtime/closures/closures.c"; "../runtime/closures/apply.S"
          ; "../runtime/io/print_int.c"; "../runtime/errors.c"
          ; "-o"; executable_path ] stdout_path stderr_path
      in
      if compilation <> Unix.WEXITED 0 then Alcotest.fail ("assembler/linker: " ^ read_file stderr_path);
      let execution = command "timeout" [ "10s"; "qemu-riscv64"; executable_path ] stdout_path stderr_path in
      (match execution with
      | Unix.WEXITED actual -> Alcotest.(check int) "exit code" status actual
      | Unix.WSIGNALED signal -> Alcotest.failf "QEMU signal %d" signal
      | Unix.WSTOPPED signal -> Alcotest.failf "QEMU stopped %d" signal);
      Alcotest.(check string) "stdout" expected (read_file stdout_path);
      Alcotest.(check string) "stderr" stderr (read_file stderr_path))
;;

let run source expected () = execute_assembly expected (generate source)

let arithmetic =
  [ "print zero", "let () = print_int 0", "0"
  ; "print maximum int64", "let x = 4611686018427387903 * 2 + 1 let () = print_int x", "9223372036854775807"
  ; "integer arithmetic", "let () = print_int ((6 * 7 + 2 - 4) / 2)", "20"
  ; "signed division", "let () = print_int ((-7) / 2)", "-3"
  ; "large literal", "let () = print_int 4000000000000", "4000000000000"
  ; "64-bit overflow", "let x = 4611686018427387903 * 4 let () = print_int x", "-4"
  ; "64-bit division overflow", "let x = (4611686018427387903 + 1) * 2 let () = print_int (x / (-1))", "-9223372036854775808"
  ; "comparisons", "let () = print_int (if (-2) < 1 && 3 > 2 && 2 <= 2 && 3 >= 3 && 4 = 4 && 4 <> 5 && not false then 1 else 0)", "1"
  ; "untaken branch", "let () = print_int (if true then 42 else 1 / 0)", "42"
  ; "short circuit", "let () = print_int (if false && 1 / 0 = 0 || true then 7 else 0)", "7"
  ; "short circuit effects", "let () = if true || (let () = print_int 9 in false) then print_int 1 else print_int 0", "1"
  ; "globals and effects", "let x = (let () = print_int 1 in 7) let f y = x + y let _ = print_int 2 let () = print_int (f 3)", "1210"
  ; "discarded local results", "let f () _ x = let _ = print_int x in let () = print_int 2 in x + 1 let () = print_int (f () 0 4)", "425"
  ; "values survive calls", "let g x = x * 2 let f x = let y = g x in let z = g 3 in x + y + z let () = print_int (f 5)", "21"
  ; "shadowing", "let x = 3 let f x = let x = x + 1 in x let () = print_int (f 5 + x)", "9"
  ; "ANF effect order", "let f a b = a + b let () = print_int (f (let () = print_int 1 in 3) (let () = print_int 2 in 4))", "217"
  ]
;;

let calls =
  [ "direct ten arguments", "let f a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j let () = print_int (f 1 2 3 4 5 6 7 8 9 10)", "55"
  ; "partial applications independent", "let add a b = a + b let f = add 1 let g = add 10 let () = print_int (f 2) let () = print_int (g 2) let () = print_int (f 3)", "3124"
  ; "function parameter", "let apply f x = f x let add a b = a + b let () = print_int (apply (add 2) 5)", "7"
  ; "builtin as value", "let apply f x = f x let () = apply print_int 42", "42"
  ; "callee from if", "let f x = x + 1 let g x = x + 2 let flag = false let h = if flag then f else g let () = print_int (h 5)", "7"
  ; "returned function and overapplication", "let add a b = a + b let choose flag = if flag then add 1 else add 2 let () = print_int (choose true 5)", "6"
  ; "local recursive capture", "let f y = let rec loop i = if i = 0 then y else loop (i - 1) in loop 10 let () = print_int (f 7)", "7"
  ; "nested closures", "let make x = fun y -> let z = x + y in fun n -> z + n let add = make 2 3 let () = print_int (add 4)", "9"
  ; "closure among direct arguments", "let invoke x f y = x + f y let id x = x let () = print_int (invoke 10 id 5)", "15"
  ; "multiple function arguments", "let apply f g x = f (g x) let inc x = x + 1 let twice x = x * 2 let () = print_int (apply inc twice 4)", "9"
  ; "indirect ten arguments", "let f a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j let apply fn = fn 1 2 3 4 5 6 7 8 9 10 let () = print_int (apply f)", "55"
  ; "partial across register boundary", "let f a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j let g = f 1 2 3 4 5 6 7 8 9 let () = print_int (g 10)", "55"
  ]
;;

let check_error source expected () =
  match Codegen.generate (lower source) with
  | Ok _ -> Alcotest.fail "expected a diagnostic"
  | Error actual -> Alcotest.(check string) "diagnostic" expected actual
;;

let check_structure () =
  let assembly = generate "let f x = x + 1 let () = print_int (f 2)" in
  Alcotest.(check bool) "direct function call" true (String.split_on_char '\n' assembly |> List.mem "\tcall bd_fn_0");
  Alcotest.(check bool) "no runtime closure allocation" false
    (String.split_on_char '\n' assembly |> List.mem "\tcall bd_make_closure");
  Alcotest.(check string) "deterministic output" assembly (generate "let f x = x + 1 let () = print_int (f 2)")
;;

let large_frame () =
  let parameters = List.init 300 (fun i -> PVar ("p" ^ string_of_int i)) in
  let function_ = ADecl (NonRec, PVar "large", parameters, ACExpr (CImm (ImmId "p299"))) in
  let body =
    ALet (PVar "result", CApp (ImmId "large", List.init 300 (fun i -> ImmInt i)),
      ACExpr (CApp (ImmId "print_int", [ ImmId "result" ])))
  in
  execute_assembly "299" (Codegen.generate [ function_; AEval body ] |> unwrap)
;;

let large_indirect_frame () =
  let parameters = List.init 300 (fun i -> PVar ("p" ^ string_of_int i)) in
  let function_ = ADecl (NonRec, PVar "large", parameters, ACExpr (CImm (ImmId "p299"))) in
  let body =
    ALet (PVar "closure", CImm (ImmId "large"),
      ALet (PVar "result", CApp (ImmId "closure", List.init 300 (fun i -> ImmInt i)),
        ACExpr (CApp (ImmId "print_int", [ ImmId "result" ]))))
  in
  execute_assembly "299" (Codegen.generate [ function_; AEval body ] |> unwrap)
;;

let safe_symbols () =
  let name = "main:\n.globl injected'" in
  let program =
    [ ADecl (NonRec, PVar name, [ PVar "x" ], ACExpr (CImm (ImmId "x")))
    ; AEval (ALet (PVar "answer", CApp (ImmId name, [ ImmInt 42 ]),
        ACExpr (CApp (ImmId "print_int", [ ImmId "answer" ])))) ]
  in
  execute_assembly "42" (Codegen.generate program |> unwrap)
;;

let frame_layout () =
  let module Frame = Allocation.Frame in
  let parameters = [ PVar "x"; PUnit; PUnderscore ] in
  let body = ALet (PVar "y", CApp (ImmId "f", List.init 20 (fun i -> ImmInt i)), ACExpr (CImm ImmUnit)) in
  let frame = Frame.create parameters [ body ] in
  Alcotest.(check int) "alignment" 0 (Frame.size frame mod Abi.stack_alignment);
  Alcotest.(check bool) "locals outside outgoing area" true (Frame.slot frame "x" > Frame.operand_slot frame);
  Alcotest.(check bool) "distinct local slots" true (Frame.slot frame "x" <> Frame.slot frame "y");
  Alcotest.(check bool) "saved ra after locals" true (Frame.slot frame "y" < Frame.size frame - Abi.word_size)
;;

let short_circuit_diagnostic () =
  match Codegen.generate [ AEval (ACExpr (CBinOp (And, ImmBool true, ImmBool false))) ] with
  | Ok _ -> Alcotest.fail "expected lowered ANF"
  | Error message ->
    Alcotest.(check string) "diagnostic"
      "RISC-V: ANF must lower short-circuit operators to CIf" message
;;

let () =
  let cases examples = List.map (fun (name, source, expected) -> Alcotest.test_case name `Quick (run source expected)) examples in
  Alcotest.run "RV64 backend"
    [ "arithmetic and state", cases arithmetic
    ; "calls and closures", cases calls
    ; "CPS",
      [ Alcotest.test_case "print_int example" `Quick (fun () -> execute_assembly "42" (generate (read_file "riscv/programs/print_int.ml")))
      ; Alcotest.test_case "factorial" `Quick (fun () -> execute_assembly "120" (generate (read_file "riscv/programs/fac_cps.ml")))
      ; Alcotest.test_case "fibonacci" `Quick (fun () -> execute_assembly "55" (generate (read_file "riscv/programs/fib_cps.ml"))) ]
    ; "ABI and diagnostics",
      [ Alcotest.test_case "large frame and 300 arguments" `Quick large_frame
      ; Alcotest.test_case "indirect call with 300 arguments" `Quick large_indirect_frame
      ; Alcotest.test_case "safe assembly symbols" `Quick safe_symbols
      ; Alcotest.test_case "frame layout" `Quick frame_layout
      ; Alcotest.test_case "direct calls and stable symbols" `Quick check_structure
      ; Alcotest.test_case "reject unlowered short circuit" `Quick short_circuit_diagnostic
      ; Alcotest.test_case "unknown name" `Quick (check_error "let () = print_endline 1" "RISC-V: unknown identifier: print_endline")
      ; Alcotest.test_case "division by zero" `Quick (fun () -> execute_assembly ~status:1 ~stderr:"division by zero\n" "" (generate "let () = print_int (1 / 0)"))
      ]
    ]
;;
