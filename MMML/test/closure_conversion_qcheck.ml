open Middleend
module Cc = Closure_conversion
module Gen = Closure_generator
module Eval = Closure_eval

let convert program =
  match Cc.convert ~externals:Eval.external_names program with
  | Ok result -> result
  | Error error -> QCheck.Test.fail_reportf "conversion failed: %a" Cc.pp_error error
;;

let semantic_equivalence =
  QCheck.Test.make
    ~name:"closure conversion: original and converted AST have the same result"
    ~count:1_000
    Gen.arbitrary
    (fun term ->
       if not (Gen.well_formed term)
       then QCheck.Test.fail_report "invalid generator output";
       let source = Gen.program term in
       let target = convert source in
       (match Closure_test_invariants.check ~externals:Eval.external_names target with
        | Ok () -> ()
        | Error error -> QCheck.Test.fail_report error);
       let before = Eval.source ~result_name:"result" source in
       let after = Eval.source ~result_name:"result" target in
       match before.outcome, after.outcome with
       | Ok _, Ok _ when before = after -> true
       | _ ->
         QCheck.Test.fail_reportf
           "source: %s\ntarget: %s\nIR: %a"
           (Eval.show_observation before)
           (Eval.show_observation after)
           Ast.pp_program
           target)
;;

let deterministic =
  QCheck.Test.make
    ~name:"closure conversion: isolated deterministic runs"
    ~count:500
    Gen.arbitrary
    (fun term ->
       let source = Gen.program term in
       let first = convert source in
       ignore
         (convert
            [ Ast.Definition
                ( Ast.Nonrecursive
                , Ast.PVariable "id"
                , Ast.Lambda (Ast.PVariable "x", Ast.Variable "x") )
            ]);
       let second = convert source in
       first = second
       && Format.asprintf "%a" Ast.pp_program first
          = Format.asprintf "%a" Ast.pp_program second)
;;

let shrinking =
  QCheck.Test.make
    ~name:"closure conversion: shrinkers preserve the semantic test domain"
    ~count:100
    Gen.arbitrary
    (fun term ->
       List.for_all
         (fun smaller ->
            Gen.well_formed smaller
            &&
            match (Eval.source ~result_name:"result" (Gen.program smaller)).outcome with
            | Ok _ -> true
            | Error _ -> false)
         (Gen.shrinks term))
;;

let () = QCheck_runner.run_tests_main [ semantic_equivalence; deterministic; shrinking ]
