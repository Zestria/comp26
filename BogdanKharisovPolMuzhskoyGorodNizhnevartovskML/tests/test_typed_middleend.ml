[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Middleend
open Middleend_test_support

let valid source stage render check value =
  match check value with
  | Ok () -> ()
  | Error error -> fail source (stage ^ ": " ^ render error)
;;

let lower_source parse source =
  let ast = parsed parse source in
  valid
    source
    "recursive check"
    Recursive_check.error_to_string
    Recursive_check.expression
    ast;
  match Lower.expression Id.initial ast with
  | Ok term -> term
  | Error error -> fail source ("lowering: " ^ Lower.error_to_string error)
;;

let check_every_pass parse input =
  let source = Typed_terms.source input in
  let expected = Typed_terms.expected input in
  let compare stage evaluate value =
    let actual = observe source evaluate value in
    if actual <> expected
    then fail source (Printf.sprintf "%s: expected %s, observed %s" stage expected actual)
  in
  let lambda, supply = lower_source parse source in
  valid source "Lambda invariant" Lambda.invariant_error_to_string Lambda.validate lambda;
  compare "Lower" Lambda_eval.observe lambda;
  let anf, supply = allocated source (Anf.of_lambda supply lambda) in
  valid source "ANF invariant" Lambda.invariant_error_to_string Anf.validate anf;
  compare "ANF" (fun term -> Lambda_eval.observe (Anf.to_lambda term)) anf;
  let closed, supply = allocated source (Closure_conversion.convert supply anf) in
  valid source "Closure invariant" Closure.error_to_string Closure.validate closed;
  compare "Closure_conversion" Anf_eval.closed closed;
  let lifted, _ = allocated source (Lambda_lifting.lift supply closed) in
  valid source "Lifted invariant" Lifted.error_to_string Lifted.validate lifted;
  compare "Lambda_lifting" Anf_eval.lifted lifted;
  true
;;

let preserves_semantics name parse =
  QCheck.Test.make
    ~name:(name ^ ": typed compositions preserve the independent result after every pass")
    ~count:1000
    Typed_terms.arbitrary
    (check_every_pass parse)
;;

let shrink_contract =
  QCheck.Test.make
    ~name:
      "typed structural shrinking preserves result type, scope and strict size decrease"
    ~count:150
    Typed_terms.arbitrary
    (fun original ->
       let source = Typed_terms.source original in
       valid source "generator scope" Fun.id Typed_terms.validate original;
       Typed_terms.shrink original (fun candidate ->
         let candidate_source = Typed_terms.source candidate in
         if not (Typed_terms.same_type original candidate)
         then fail source ("Shrinker changed the result type: " ^ candidate_source);
         if Typed_terms.size candidate >= Typed_terms.size original
         then fail source ("Shrinker did not decrease its measure: " ^ candidate_source);
         valid candidate_source "shrunk scope" Fun.id Typed_terms.validate candidate;
         (* Exercise printed lexical names too, including alpha-renaming required
            by substitutions across a same-spelled inner binding. *)
         let lowered, _ =
           lower_source Frontend.Parser.parse_expression candidate_source
         in
         let actual = observe candidate_source Lambda_eval.observe lowered in
         let _ = equal candidate_source (Typed_terms.expected candidate) actual in
         ());
       true)
;;

(* A real faulty IR transformation: every subtraction primitive becomes addition,
   including inside function bodies. The oracle does not see this mutation. *)
let mutant input =
  let source = Typed_terms.source input in
  let atom = function
    | Anf.Primitive Primitive.Subtract -> Anf.Primitive Primitive.Add
    | atom -> atom
  in
  let rec function_ (definition : Anf.function_definition) =
    { definition with body = Anf.map ~atom ~function_ definition.body }
  in
  let lambda, supply = lower_source Frontend.Parser.parse_expression source in
  let anf, _ = allocated source (Anf.of_lambda supply lambda) in
  let changed = Anf.map ~atom ~function_ anf in
  observe source Lambda_eval.observe (Anf.to_lambda changed)
;;

let shrinking_finds_counterexample =
  QCheck.Test.make
    ~name:
      "QCheck minimizes an arithmetic IR mutation while retaining a valid failing term"
    ~count:1
    QCheck.unit
    (fun () ->
       let initial = Typed_terms.shrink_witness () in
       let _ = check_every_pass Frontend.Parser.parse_expression initial in
       let arb =
         QCheck.make
           ~print:Typed_terms.source
           ~small:Typed_terms.size
           ~shrink:Typed_terms.shrink
           (QCheck.Gen.return initial)
       in
       let agrees term = mutant term = Typed_terms.expected term in
       let cell = QCheck.Test.make_cell ~count:1 arb agrees in
       match QCheck.TestResult.get_state (QCheck.Test.check_cell cell) with
       | Failed { instances = [ { instance = minimal; shrink_steps; _ } ] } ->
         let source = Typed_terms.source minimal in
         valid source "minimal scope" Fun.id Typed_terms.validate minimal;
         if shrink_steps = 0 || Typed_terms.size minimal >= Typed_terms.size initial
         then fail source "QCheck did not shrink the failing input";
         if (not (Typed_terms.same_type initial minimal)) || agrees minimal
         then
           fail
             source
             "Shrinking lost the original type or stopped reproducing the defect";
         Typed_terms.shrink minimal (fun candidate ->
           if not (agrees candidate)
           then
             fail
               source
               ("A smaller failing candidate remains: " ^ Typed_terms.source candidate));
         check_every_pass Frontend.Parser.parse_expression minimal
       | _ ->
         fail
           (Typed_terms.source initial)
           "Expected one minimized mutation counterexample")
;;

let live_combinations =
  QCheck.Test.make
    ~name:
      "generated features combine on executed paths, including partial and captured calls"
    ~count:1
    QCheck.unit
    (fun () ->
       let random = Random.State.make [| 48137 |] in
       let required = [ "function"; "partial"; "shadow"; "if"; "tuple" ] in
       let rec sample remaining combined captured overapplied =
         if remaining = 0
         then combined, captured, overapplied
         else (
           let input = Typed_terms.arbitrary.gen random in
           let source = Typed_terms.source input in
           valid source "generated bounds and scope" Fun.id Typed_terms.validate input;
           let live = Typed_terms.execution_features input in
           let includes feature = List.mem feature live in
           sample
             (remaining - 1)
             (combined + if List.for_all includes required then 1 else 0)
             (captured + if includes "capture" then 1 else 0)
             (overapplied + if includes "overapplication" then 1 else 0))
       in
       let combined, captured, overapplied = sample 512 0 0 0 in
       if combined < 8 || captured < 20 || overapplied < 50
       then
         fail
           "generator seed 48137, 512 inputs"
           (Printf.sprintf
              "Insufficient live combinations: all five=%d (need 8), captured calls=%d \
               (need 20), overapplications=%d (need 50)"
              combined
              captured
              overapplied);
       true)
;;

let () =
  run_tests
    [ preserves_semantics "Angstrom" Frontend.Parser.parse_expression
    ; preserves_semantics "Menhir" Frontend.Menhir_parser.parse_expression
    ; shrink_contract
    ; shrinking_finds_counterexample
    ; live_combinations
    ]
;;
