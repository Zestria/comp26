[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Middle-end tests for MyLittleML (Task 2).

    CPS factorial and fibonacci (the same sources as [Test_qc]'s targeted
    assertions) are parsed with both parsers, run through every pipeline stage
    and evaluated with {!Middleend.Eval}:

    - stage invariants hold ([Anf.is_cexp_rhs] after ANF, [Closure.is_closed]
      after closure conversion, [Lambda_lift.is_lifted] after lifting);
    - all four stages evaluate to the same integers (factorial of 5 is 120,
      fibonacci of 10 is 55).

    QCheck generators follow the [Test_qc] conventions: identifier pools and
    explicitly bounded sizes. *)

open Mylittleml.Frontend
open Mylittleml.Middleend

(* CPS programs (same as Test_qc) ------------------------------------------ *)

let fact_src =
  "let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n))"
;;

let fib_src =
  "let rec fib n k = if n <= 1 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b -> \
   k (a + b)))"
;;

(** A whole program calling the CPS function with the identity continuation. *)
let driver src fname arg = Printf.sprintf "%s;;\n%s %d (fun x -> x)" src fname arg

let fact_prog5 = driver fact_src "fact" 5
let fib_prog10 = driver fib_src "fib" 10

(* Helpers ------------------------------------------------------------------ *)

let parse_exn kind src =
  match Parse.parse kind { Parse.sticky = false } src with
  | Ok prog -> prog
  | Error msg -> failwith ("parse error: " ^ msg)
;;

(** Run one program through all four stages, returning the ANF programs. *)
let stages prog =
  let anf = Anf.anf_prog prog in
  let cc = Closure.convert_prog anf in
  let ll = Lambda_lift.lift_prog cc in
  anf, cc, ll
;;

let check_invariants name (anf, cc, ll) =
  let rhs_ok =
    List.for_all (function
      | Anf.ADecl (_, _, rhs) -> Anf.is_cexp_rhs rhs
      | Anf.AExp e -> Anf.is_cexp_rhs e)
  in
  if not (rhs_ok anf)
  then failwith (name ^ ": ANF flatness invariant violated (is_cexp_rhs)");
  if not (Closure.is_closed cc)
  then failwith (name ^ ": closure invariant violated (is_closed)");
  if not (Lambda_lift.is_lifted ll)
  then failwith (name ^ ": lifting invariant violated (is_lifted)")
;;

let as_int = function
  | Eval.VInt n -> n
  | Eval.VBool _ -> failwith "expected int, got bool"
  | Eval.VClos _ -> failwith "expected int, got closure"
;;

let check_semantics name src expected =
  List.iter
    (fun kind ->
       let kname =
         match kind with
         | Parse.Topdown -> "topdown"
         | Parse.Bottomup -> "bottomup"
       in
       let prog = parse_exn kind src in
       let anf, cc, ll = stages prog in
       check_invariants (name ^ " (" ^ kname ^ ")") (anf, cc, ll);
       let got_src = as_int (Eval.eval_src prog) in
       let got_anf = as_int (Eval.eval_anf anf) in
       let got_cc = as_int (Eval.eval_cc cc) in
       let got_ll = as_int (Eval.eval_ll ll) in
       if
         got_src <> expected
         || got_anf <> expected
         || got_cc <> expected
         || got_ll <> expected
       then
         failwith
           (Format.sprintf
              "%s (%s): expected %d, got src=%d anf=%d cc=%d ll=%d"
              name
              kname
              expected
              got_src
              got_anf
              got_cc
              got_ll))
    [ Parse.Topdown; Parse.Bottomup ]
;;

let assert_test name f =
  QCheck.Test.make ~name (QCheck.make QCheck.Gen.unit) (fun () ->
    f ();
    true)
;;

(* Suite --------------------------------------------------------------------- *)

let () =
  exit
    (QCheck_runner.run_tests
       ~colors:false
       [ assert_test "cps factorial evaluates to 120 on all stages" (fun () ->
           check_semantics "fact 5" fact_prog5 120)
       ; assert_test "cps fibonacci evaluates to 55 on all stages" (fun () ->
           check_semantics "fib 10" fib_prog10 55)
       ])
;;
