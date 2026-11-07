[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** State, parser, backend and evaluator purity tests for MyLittleML.

    Every check is a targeted assertion rather than a generator: the same
    [State_error] computation is run twice, two parses of the same source
    are compared, and two compilations of the same program are compared
    for byte-identical assembly.  The middle-end and reference evaluator
    are exercised on the same CPS-style sources as [Test_qc], and the
    stage results are required to agree.

    - [State_error] threads immutable state, short-circuits on failure and
      leaves the initial state untouched when a run fails;
    - [Middleend.Names] skips collisions and has independent runs;
    - parser diagnostics and backend errors do not leak between calls;
    - IL and calling-convention errors are returned as [result] values;
    - nested [let], fresh closure-conversion names, local recursive
      closures and partial application preserve continuations;
    - the reference evaluator reports division by zero, non-function
      applications and cyclic non-function values.

    No generators: this module is the fixed-shape counterpart of
    [Test_qc]'s randomised properties. *)

open Mylittleml
module M = State_error
module P = Frontend.Parse
module E = Middleend.Eval

let parse source = P.parse P.Topdown { P.sticky = false } source

let compile source =
  Result.bind (parse source) (fun ast ->
    Result.bind
      (Riscv.Codegen.from_aprogram (Middleend.Anf.anf_prog ast))
      (fun il -> Result.map Riscv.Asm.emit (Riscv.Codegen.compile_program il)))
;;

let check name test = QCheck.Test.make ~name ~count:1 (QCheck.make QCheck.Gen.unit) test

let is_error = function
  | Error _ -> true
  | Ok _ -> false
;;

let is_int expected = function
  | Ok (E.VInt n) -> n = expected
  | _ -> false
;;

let stages_equal source expected =
  match parse source with
  | Error _ -> false
  | Ok ast ->
    let anf = Middleend.Anf.anf_prog ast in
    let cc = Middleend.Closure.convert_prog anf in
    let ll = Middleend.Lambda_lift.lift_prog cc in
    List.for_all
      (is_int expected)
      [ E.eval_src ast; E.eval_anf anf; E.eval_cc cc; E.eval_ll ll ]
;;

let eval_source source = Result.bind (parse source) E.eval_src

let deterministic source =
  match compile source, compile source with
  | Ok a, Ok b -> String.equal a b
  | _ -> false
;;

let tests =
  [ check "State_error threads immutable state" (fun () ->
      let open M in
      let computation =
        let* n = get in
        let* () = put (n + 1) in
        return n
      in
      match computation 41 with
      | Ok (41, 42) -> true
      | _ -> false)
  ; check "State_error short-circuits and retains failure state" (fun () ->
      let open M in
      let computation =
        let* () = put 42 in
        let* () = fail "stop" in
        put 999
      in
      match computation 0 with
      | Error ("stop", 42) -> true
      | _ -> false)
  ; check "State_error failed run cannot alter initial state" (fun () ->
      let open M in
      let initial = 7 in
      let computation =
        let* () = put 99 in
        fail "stop"
      in
      match computation initial, get initial with
      | Error ("stop", 99), Ok (7, 7) -> true
      | _ -> false)
  ; check "fresh names skip collisions and have independent runs" (fun () ->
      let module N = Middleend.Names in
      let initial = N.make (N.StringSet.singleton "v0") in
      let first, next = N.fresh "v" initial in
      let second, _ = N.fresh "v" next in
      let again, _ = N.fresh "v" initial in
      first = "v1" && second = "v2" && again = "v1")
  ; check "parser diagnostics do not leak between calls" (fun () ->
      match parse "let x =", parse "1", parse "let x =" with
      | Error a, Ok _, Error b -> String.equal a b
      | _ -> false)
  ; check "backend errors do not poison next compilation" (fun () ->
      is_error (compile "print_int missing") && deterministic "print_int 42")
  ; check "backend rejects recursive non-functions through Error" (fun () ->
      is_error (compile "let rec x = 1;; x"))
  ; check "IL errors are values" (fun () ->
      is_error
        (Riscv.Codegen.compile_program
           [ { Riscv.Program.name = "main"
             ; params = []
             ; body = Riscv.Program.Var "missing"
             }
           ]))
  ; check "invalid register index is a value" (fun () ->
      is_error (Riscv.Callconv.arg_reg (-1)) && is_error (Riscv.Callconv.arg_reg 6))
  ; check "nested let in an operand preserves its continuation" (fun () ->
      stages_equal "1 + (let x = 2 in x + 3)" 6)
  ; check "fresh CC names avoid user names in expression phrases" (fun () ->
      stages_equal "let cc_f0 = 2 in let f x = cc_f0 + x in f 40" 42)
  ; check "local recursive closure captures environment" (fun () ->
      stages_equal
        "let outer x = let rec loop n = if n = 0 then x else loop (n - 1) in loop 3;; \
         outer 42"
        42)
  ; check "partial application shares recursive addresses correctly" (fun () ->
      stages_equal
        "let rec f x n = if n = 0 then x else f x (n - 1);; let g = f 42;; g 5"
        42)
  ; check "reference evaluator reports division by zero" (fun () ->
      match eval_source "1 / 0" with
      | Error "division by zero" -> true
      | _ -> false)
  ; check "reference evaluator reports non-function applications" (fun () ->
      is_error (eval_source "1 2"))
  ; check "reference evaluator detects cyclic non-functions" (fun () ->
      match eval_source "let rec x = x in x" with
      | Error "cyclic non-function value" -> true
      | _ -> false)
  ; check "failed evaluation cannot leak a recursive binding" (fun () ->
      is_error (eval_source "let rec x = x in x") && is_int 42 (eval_source "42"))
  ]
;;

let () = exit (QCheck_runner.run_tests ~colors:false tests)
