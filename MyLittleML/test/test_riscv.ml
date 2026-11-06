[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** RISC-V end-to-end tests for MyLittleML.

    CPS factorial and fibonacci (the same sources as [Test_qc]'s targeted
    assertions) are compiled to RISC-V assembly with both parsers, statically
    linked into executables and run under QEMU:

    - the printed output and the exit code match the expectations recorded
      in {!test_case} (factorial of 10 is 3628800, fibonacci of 15 is 610);
    - signed division and modulo, 64-bit boundaries and division by zero
      (exit code 2) behave as specified;
    - a function with 300 local variables exercises stack offsets beyond
      the signed 12-bit range.

    A standalone executable invoked by dune via a [rule]
    on the [runtest] alias, with the compiler path passed as [Sys.argv.(1)].
    Failures are threaded through [result] and reported by {!exit_with}. *)

type test_case =
  { name : string
  ; source : string
  ; expected : string
  ; exit_code : int (* expected exit code, defaults to 0 *)
  }

(* ---------- Locating tools in PATH ---------- *)

(* Return the full path of [tool] if it exists in PATH, otherwise None. *)
let which tool =
  let paths =
    String.split_on_char ':' (Option.value ~default:"" (Sys.getenv_opt "PATH"))
  in
  let candidates = List.map (fun dir -> Filename.concat dir tool) paths in
  List.find_opt Sys.file_exists candidates
;;

(* Resolve a tool: look at the environment variable [name], fall back to
   [default], and check that the resulting binary is present in PATH. *)
let require_tool ~name ~default =
  let tool = Option.value ~default (Sys.getenv_opt name) in
  match which tool with
  | Some _ -> Ok tool
  | None -> Error (Printf.sprintf "Required tool missing: %s" tool)
;;

(* ---------- Test generation ---------- *)

let fact =
  "let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n));; "
;;

let fib =
  "let rec fib n k = if n <= 1 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b ->  \n\
  \   k (a + b)));; "
;;

(* Hand-written test cases. *)
let base_cases =
  [ { name = "fact0"; source = fact ^ "fact 0 print_int"; expected = "1"; exit_code = 0 }
  ; { name = "fact10"
    ; source = fact ^ "fact 10 print_int"
    ; expected = "3628800"
    ; exit_code = 0
    }
  ; { name = "fact20"
    ; source = fact ^ "fact 20 print_int"
    ; expected = "2432902008176640000"
    ; exit_code = 0
    }
  ; { name = "fib0"; source = fib ^ "fib 0 print_int"; expected = "0"; exit_code = 0 }
  ; { name = "fib1"; source = fib ^ "fib 1 print_int"; expected = "1"; exit_code = 0 }
  ; { name = "fib10"; source = fib ^ "fib 10 print_int"; expected = "55"; exit_code = 0 }
  ; { name = "fib15"; source = fib ^ "fib 15 print_int"; expected = "610"; exit_code = 0 }
  ; { name = "negative"
    ; source = "print_int (-123456789)"
    ; expected = "-123456789"
    ; exit_code = 0
    }
  ; { name = "zero"; source = "print_int 0"; expected = "0"; exit_code = 0 }
  ; { name = "int64min"
    ; source = "print_int ((0 - 4611686018427387903 - 1) * 2)"
    ; expected = "-9223372036854775808"
    ; exit_code = 0
    }
  ; { name = "int64max"
    ; source = "print_int (4611686018427387903 * 2 + 1)"
    ; expected = "9223372036854775807"
    ; exit_code = 0
    }
  ; { name = "large_immediates"
    ; source = "let f x = x + 500000 - 1000000;; print_int (f 7)"
    ; expected = "-499993"
    ; exit_code = 0
    }
  ; { name = "sub"
    ; source = "let f x = x - 1;; print_int (f 42)"
    ; expected = "41"
    ; exit_code = 0
    }
  ; { name = "partial"
    ; source = "let add x y = x + y;; let inc = add 1;; print_int (inc 41)"
    ; expected = "42"
    ; exit_code = 0
    }
  ; { name = "unknown_arity"
    ; source =
        "let apply f x y = f x y;; let add x y = x + y;; print_int (apply add 20 22)"
    ; expected = "42"
    ; exit_code = 0
    }
  ; { name = "overapply"
    ; source =
        "let f x = if x > 0 then (fun y -> x + y) else (fun y -> y);; print_int (f 10 32)"
    ; expected = "42"
    ; exit_code = 0
    }
  ; { name = "nested_rec"
    ; source =
        "let outer x = let rec loop n = if n = 0 then x else loop (n - 1) in loop 20;;  \n\
        \         print_int (outer 42)"
    ; expected = "42"
    ; exit_code = 0
    }
  ; { name = "shadowing"
    ; source = "let x = 10;; let f y = x + y;; let x = 100;; print_int (f 32)"
    ; expected = "42"
    ; exit_code = 0
    }
  ; { name = "shadow_function"
    ; source = "let f x = x + 1;; let g f = f 10;; print_int (g (fun x -> x * 2))"
    ; expected = "20"
    ; exit_code = 0
    }
  ; { name = "print_alias"
    ; source = "let p = print_int;; p 42"
    ; expected = "42"
    ; exit_code = 0
    }
  ; { name = "print_shadow"
    ; source = "let print_int x = x + 1;; let x = print_int 41;; x"
    ; expected = ""
    ; exit_code = 0
    }
  ; { name = "effects_once"
    ; source = "let f x = let z = print_int x in x;; let a = f 7 in print_int (a + a)"
    ; expected = "714"
    ; exit_code = 0
    }
  ; { name = "unused_effect"
    ; source = "let f x = print_int x;; let unused = f 9;; print_int 1"
    ; expected = "91"
    ; exit_code = 0
    }
  ; { name = "partial_effect"
    ; source =
        "let f x = let z = print_int x in fun y -> x + y;; let g = f 7;; let z =  \n\
        \         print_int 8;; print_int (g 2)"
    ; expected = "789"
    ; exit_code = 0
    }
  ; { name = "branch_labels"
    ; source =
        "let f x = if x < 0 then 1 else 2;; let g x = if x < 0 then 3 else 4;; print_int  \n\
        \         (f 1 + g (-1))"
    ; expected = "5"
    ; exit_code = 0
    }
  ; { name = "nested_calls"
    ; source = "let f x = x * 2;; print_int (f 10 + f (f 3))"
    ; expected = "32"
    ; exit_code = 0
    }
  ; { name = "underscores"
    ; source = "let f_1 x = x + 1;; print_int (f_1 41)"
    ; expected = "42"
    ; exit_code = 0
    }
  ; { name = "symbol_collision"
    ; source = "let malloc x = x + 1;; let main x = malloc x;; print_int (main 41)"
    ; expected = "42"
    ; exit_code = 0
    }
  ; { name = "many_args"
    ; source =
        "let f a b c d e f g h i j = a+b+c+d+e+f+g+h+i+j;; print_int (f 1 2 3 4 5 6 7 8  \n\
        \         9 10)"
    ; expected = "55"
    ; exit_code = 0
    }
  ; { name = "many_captures"
    ; source =
        "let f a b c d e f g h i j = fun x -> a+b+c+d+e+f+g+h+i+j+x;; print_int (f 1 2 3  \n\
        \         4 5 6 7 8 9 10 1)"
    ; expected = "56"
    ; exit_code = 0
    }
  ]
;;

(* Auto-generate tests for every binary operator. Comparisons are wrapped in
   [if ... then 1 else 0] so the printed value is 0 or 1. *)
let arith_cases =
  let ops =
    [ "+", "10"
    ; "-", "4"
    ; "*", "21"
    ; "/", "2"
    ; "mod", "1"
    ; "<", "0"
    ; "<=", "0"
    ; ">", "1"
    ; ">=", "1"
    ; "=", "0"
    ; "<>", "1"
    ]
  in
  let is_cmp op = List.mem op [ "<"; "<="; ">"; ">="; "="; "<>" ] in
  List.mapi
    (fun i (op, expected) ->
       let expr = Printf.sprintf "7 %s 3" op in
       let expr = if is_cmp op then Printf.sprintf "if %s then 1 else 0" expr else expr in
       { name = Printf.sprintf "op%d" i
       ; source = Printf.sprintf "print_int (%s)" expr
       ; expected
       ; exit_code = 0
       })
    ops
;;

(* Signed division/remainder semantics, plus division by zero (exit code 2). *)
let tail_cases =
  [ { name = "signed_div"
    ; source = "print_int ((-7) / 3)"
    ; expected = "-2"
    ; exit_code = 0
    }
  ; { name = "signed_rem"
    ; source = "print_int ((-7) mod 3)"
    ; expected = "-1"
    ; exit_code = 0
    }
  ; { name = "divzero"; source = "print_int (1 / 0)"; expected = ""; exit_code = 2 }
  ; { name = "modzero"; source = "print_int (1 mod 0)"; expected = ""; exit_code = 2 }
  ]
;;

(* A function with 300 local variables. This forces stack offsets and frame
   adjustments beyond the signed 12-bit range, exercising large-frame codegen. *)
let large_frame_case =
  let parts = List.init 300 (fun i -> Printf.sprintf "let x%d = id %d in " i i) in
  let sum_expr = String.concat "+" (List.init 300 (fun i -> Printf.sprintf "x%d" i)) in
  let source =
    "let id x = x;; " ^ String.concat "" parts ^ "print_int (" ^ sum_expr ^ ")"
  in
  let expected = string_of_int (List.fold_left ( + ) 0 (List.init 300 (fun i -> i))) in
  { name = "large_frame"; source; expected; exit_code = 0 }
;;

let all_cases = base_cases @ arith_cases @ tail_cases @ [ large_frame_case ]

(* ---------- Monadic helpers for [result] ---------- *)

(* Bind for the [result] type, so fallible steps can be chained without
   nested matches. *)
let ( let* ) r f =
  match r with
  | Ok x -> f x
  | Error _ as e -> e
;;

(* ---------- Running a single external command ---------- *)

type command_output =
  { stdout : string
  ; exit_code : int
  }

type run_result =
  | Ok_run of command_output
  | Spawn_error of string

(* Run [argv] and capture its stdout and exit code. stderr is discarded so it
   does not pollute the checked output. *)
let run_command argv =
  let cmd = String.concat " " (List.map Filename.quote argv) in
  let ic = Unix.open_process_in (cmd ^ " 2>/dev/null") in
  let stdout = In_channel.input_all ic in
  let exit_code =
    match Unix.close_process_in ic with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 128
  in
  Ok_run { stdout; exit_code }
;;

(* ---------- Executing one test for one parser ---------- *)

type test_result =
  | Pass
  | Fail of string

(* Compile [case] with [compiler], link it statically for RISC-V, run it under
   QEMU, and compare stdout and exit code against the expectations. *)
let run_one_test ~compiler ~cc ~qemu ~parser ~root (case : test_case) =
  let ml = Filename.concat root (case.name ^ ".ml") in
  let asm = Filename.concat root (case.name ^ ".s") in
  let exe = Filename.concat root (case.name ^ ".exe") in
  (* Write the source file, guaranteeing the channel is closed. *)
  let write_file path contents =
    let oc = open_out path in
    Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc contents)
  in
  (* No sequential operator: use [let () = ... in]. *)
  let () = write_file ml case.source in
  (* The three build/run steps, as a plain data list rather than a chain of
     nested matches. *)
  let steps =
    [ "compile", [ compiler; "-parsing"; parser; "--backend"; "riscv"; ml; "-o"; asm ]
    ; "link", [ cc; "-static"; "-march=rv64gc"; "-mabi=lp64d"; asm; "-o"; exe ]
    ; "qemu", [ qemu; exe ]
    ]
  in
  (* Run the steps in order, stopping at the first failure. A non-zero exit
     code is only an error for build steps; the qemu step is checked against
     [case.exit_code] below. *)
  let outcome =
    List.fold_left
      (fun acc (label, argv) ->
         let* _last = acc in
         match run_command argv with
         | Spawn_error msg ->
           Error (Printf.sprintf "%s/%s: %s: %s" parser case.name label msg)
         | Ok_run { exit_code = n; _ } when label <> "qemu" && n <> 0 ->
           Error (Printf.sprintf "%s/%s: %s failed (exit %d)" parser case.name label n)
         | Ok_run r -> Ok r)
      (Ok { stdout = ""; exit_code = 0 })
      steps
  in
  match outcome with
  | Error msg -> Fail msg
  | Ok { stdout; exit_code } ->
    if exit_code <> case.exit_code || stdout <> case.expected
    then
      Fail
        (Printf.sprintf
           "%s/%s: expected %S, got %S, exit=%d (expected exit=%d)"
           parser
           case.name
           case.expected
           stdout
           exit_code
           case.exit_code)
    else Pass
;;

(* ---------- Main ---------- *)

(* Print [msg] to stderr and terminate with [code]. Uses [let () = ... in]
   instead of the sequential operator [;]. *)
let exit_with ~code msg =
  let () = prerr_endline msg in
  exit code
;;

let () =
  let compiler =
    if Array.length Sys.argv < 2
    then exit_with ~code:2 "usage: test_runner <compiler>"
    else Sys.argv.(1)
  in
  let outcome =
    let* cc = require_tool ~name:"RISCV_CC" ~default:"riscv64-linux-gnu-gcc" in
    let* qemu = require_tool ~name:"QEMU_RISCV64" ~default:"qemu-riscv64" in
    (* Create a temporary directory to hold all intermediate files. *)
    let tmp = Filename.temp_file "mylittleml-rv64-" "" in
    let () = Sys.remove tmp in
    let () = Unix.mkdir tmp 0o700 in
    let root = tmp in
    let parsers = [ "topdown"; "menhir" ] in
    (* Run every (parser, case) pair and collect the results. *)
    let results =
      List.concat_map
        (fun parser ->
           List.map (run_one_test ~compiler ~cc ~qemu ~parser ~root) all_cases)
        parsers
    in
    let failures =
      List.filter_map
        (function
          | Fail m -> Some m
          | Pass -> None)
        results
    in
    let total = List.length results in
    if failures = []
    then Ok (Printf.sprintf "RV64: %d QEMU tests passed (both parsers)" total)
    else Error (String.concat "\n" failures)
  in
  match outcome with
  | Ok msg -> print_endline msg
  | Error msg -> exit_with ~code:1 msg
;;
