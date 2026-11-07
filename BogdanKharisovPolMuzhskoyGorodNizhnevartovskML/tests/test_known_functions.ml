[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend
open Bkml_middleend
open Bkml_middleend_test_support

let compile source =
  match Pipeline.compile_expression (parsed Parser.parse_expression source) with
  | Ok program -> program
  | Error error -> fail source (Pipeline.error_to_string error)
;;

type counts =
  { closures : int
  ; direct : int
  ; indirect : int
  }

let counts (program : Lifted.program) =
  let term =
    Anf.fold
      ~atom:(fun counts _ -> counts)
      ~callee:(fun counts target ~arity:_ ->
        match target with
        | Lifted.Direct _ -> { counts with direct = counts.direct + 1 }
        | Indirect _ -> { counts with indirect = counts.indirect + 1 })
      ~binder:(fun counts _ -> counts)
      ~function_:(fun counts _ -> { counts with closures = counts.closures + 1 })
  in
  List.fold_left
    (fun counts (definition : Lifted.function_definition) -> term counts definition.body)
    (term { closures = 0; direct = 0; indirect = 0 } program.body)
    program.functions
;;

let environments_empty (program : Lifted.program) =
  List.for_all
    (fun (definition : Lifted.function_definition) -> definition.environment = [])
    program.functions
;;

let check name source expected shape =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let program = compile source in
    let _ = equal source expected (observe source Anf_eval.lifted program) in
    shape program
    || fail
         source
         ("Unexpected optimized structure:\n"
          ^ Format.asprintf "%a" Lifted_print.program program))
;;

let no_value_closures program =
  let counts = counts program in
  environments_empty program && counts.closures = 0 && counts.direct > 0
;;

let retains_values program = (counts program).closures > 0
let retains_environment program = not (environments_empty program)

let examples =
  [ check
      "known closed helper and its caller need neither captures nor value closures"
      "let inc x = x + 1 in let twice x = inc (inc x) in twice 40"
      "42"
      no_value_closures
  ; check
      "callee-only self recursion needs no self capture"
      "let rec f n = if n = 0 then 42 else f (n - 1) in f 5"
      "42"
      no_value_closures
  ; check
      "self demand passes through CPS continuation captures"
      (cps_fibonacci 6)
      "8"
      (fun program ->
         (* The recursive function's code has two declared parameters. Its
           captured self can disappear even though continuations need data. *)
         List.exists
           (fun (definition : Lifted.function_definition) ->
              List.length definition.parameters = 1 && definition.environment = [])
           program.functions
         && (counts program).direct > 0)
  ; check
      "a function used as an argument retains its value"
      "let inc x = x + 1 in let apply f = f 41 in apply inc"
      "42"
      retains_values
  ; check
      "partial application retains its function value"
      "let add x y = x + y in let add20 = add 20 in add20 22"
      "42"
      retains_values
  ; check
      "overapplication stays generic at the declared arity boundary"
      "let f x = let y = x + 1 in fun z -> y + z in f 20 21"
      "42"
      (fun program ->
         match program.body with
         | Let (_, Function _, Return (Apply (Indirect _, _, [ _ ]))) -> true
         | _ -> false)
  ; check
      "partial self application keeps the recursive environment"
      "let rec f n acc = if n = 0 then acc else let g = f (n - 1) in g (acc + 1) in f 4 0"
      "4"
      retains_environment
  ; check
      "passing self as a value keeps the recursive environment"
      "let apply f x = f x in let rec f n = if n = 0 then 42 else apply f (n - 1) in f 3"
      "42"
      retains_environment
  ; check
      "known code does not imply a common factory environment"
      "let make x = let add y = x + y in let call y = add y in call in let a = make 10 \
       in let b = make 100 in a 1 + b 1"
      "112"
      retains_environment
  ; check
      "removing helper slots preserves the intervening data capture"
      "let inc x = x + 1 in let make a = let dec x = x - 1 in fun b -> inc (a + dec b) \
       in let g = make 20 in g 22"
      "42"
      (fun program ->
         retains_environment program
         && List.for_all
              (fun (definition : Lifted.function_definition) ->
                 List.length definition.environment <= 1)
              program.functions)
  ; check
      "function shadowing selects code by binding identity"
      "let inc x = x + 1 in let first x = inc x in let inc x = x + 100 in first 1 + inc 1"
      "103"
      no_value_closures
  ; check
      "direct calls preserve an eager effectful recursive initializer"
      "let rec f = let _ = 1 / 0 in fun x -> x + 1 in f 41"
      "Division_by_zero"
      (fun program -> (counts program).direct > 0)
  ; check
      "unused syntactic recursive functions do not keep themselves alive"
      "let rec unused x = unused x in 42"
      "42"
      (fun program -> program.functions = [] && (counts program).closures = 0)
  ; check
      "returned closed function values remain available"
      "let inc x = x + 1 in inc"
      "<function>"
      retains_values
  ]
;;

let integer n =
  let text = string_of_int n in
  match Integer_literal.of_string text with
  | Ok literal -> Anf.Constant (Int literal)
  | Error error -> fail text (Integer_literal.error_to_string error)
;;

let identity supply name =
  let code, supply = fresh supply name in
  let parameter, supply = fresh supply "arg" in
  let definition : Lifted.function_definition =
    { code
    ; environment = []
    ; parameter
    ; parameters = []
    ; body = Return (Atom (Var parameter))
    }
  in
  definition, supply
;;

let validate name program =
  match Lifted.validate program with
  | Ok () -> ()
  | Error error -> fail name (Lifted.error_to_string error)
;;

let multiple_sites supply same =
  let first, supply = identity supply "first" in
  let second, supply = identity supply "second" in
  let second =
    { second with
      body =
        Return (Apply (Indirect (Primitive Add), Var second.parameter, [ integer 1 ]))
    }
  in
  let slot, supply = fresh supply "captured_function" in
  let apply, supply = identity supply "apply" in
  let apply = { apply with environment = [ slot ] } in
  let apply =
    { apply with body = Return (Apply (Indirect (Var slot), Var apply.parameter, [])) }
  in
  let f, supply = fresh supply "f" in
  let g, supply = fresh supply "g" in
  let a, supply = fresh supply "a" in
  let b, supply = fresh supply "b" in
  let av, supply = fresh supply "av" in
  let bv, supply = fresh supply "bv" in
  let program : Lifted.program =
    { functions = [ first; second; apply ]
    ; body =
        Let
          ( f
          , Function { code = first.code; captures = [] }
          , Let
              ( g
              , Function { code = second.code; captures = [] }
              , Let
                  ( a
                  , Function { code = apply.code; captures = [ Var f ] }
                  , Let
                      ( b
                      , Function
                          { code = apply.code
                          ; captures = [ Var (if same then f else g) ]
                          }
                      , Let
                          ( av
                          , Apply (Indirect (Var a), integer 20, [])
                          , Let
                              ( bv
                              , Apply (Indirect (Var b), integer 22, [])
                              , Return
                                  (Apply (Indirect (Primitive Add), Var av, [ Var bv ]))
                              ) ) ) ) ) )
    }
  in
  (program, apply.code), supply
;;

let sites name same =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let (source, code), _ = multiple_sites Id.initial same in
    validate name source;
    let program = Known_functions.optimize source in
    validate name program;
    let expected = if same then "42" else "43" in
    let _ = equal name expected (observe name Anf_eval.lifted program) in
    let definition =
      List.find
        (fun (d : Lifted.function_definition) -> Id.equal d.code code)
        program.functions
    in
    definition.environment = [] = same
    || fail name "All incoming capture identities must agree before specializing a slot")
;;

let invalid name make expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    match Lifted.validate (make ()) with
    | Error error when expected error -> true
    | Error error -> fail name ("Unexpected error: " ^ Lifted.error_to_string error)
    | Ok () -> fail name "Expected direct-call validation to fail")
;;

let invariants =
  [ invalid
      "direct code must be declared"
      (fun () ->
         let absent, _ = fresh Id.initial "absent" in
         { Lifted.functions = []; body = Return (Apply (Direct absent, integer 1, [])) })
      (function
        | Unknown_code _ -> true
        | _ -> false)
  ; invalid
      "direct code cannot require an environment"
      (fun () ->
         let definition, supply = identity Id.initial "code" in
         let slot, _ = fresh supply "slot" in
         let definition = { definition with environment = [ slot ] } in
         { Lifted.functions = [ definition ]
         ; body = Return (Apply (Direct definition.code, integer 1, []))
         })
      (function
        | Direct_environment _ -> true
        | _ -> false)
  ; invalid
      "direct calls must supply the declared arity"
      (fun () ->
         let definition, _ = identity Id.initial "code" in
         { Lifted.functions = [ definition ]
         ; body = Return (Apply (Direct definition.code, integer 1, [ integer 2 ]))
         })
      (function
        | Call_arity (_, 1, 2) -> true
        | _ -> false)
  ; invalid
      "direct calls cannot omit a declared argument"
      (fun () ->
         let definition, supply = identity Id.initial "code" in
         let second, _ = fresh supply "second" in
         let definition = { definition with parameters = [ second ] } in
         { Lifted.functions = [ definition ]
         ; body = Return (Apply (Direct definition.code, integer 1, []))
         })
      (function
        | Call_arity (_, 2, 1) -> true
        | _ -> false)
  ; invalid
      "direct arguments still obey lexical scope"
      (fun () ->
         let definition, supply = identity Id.initial "code" in
         let missing, _ = fresh supply "missing" in
         { Lifted.functions = [ definition ]
         ; body = Return (Apply (Direct definition.code, Var missing, []))
         })
      (function
        | Binding (Unbound_variable _) -> true
        | _ -> false)
  ]
;;

let unknown_cycle =
  let name = "a seedless capture cycle cannot confirm another site's known code" in
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let (source, code), supply = multiple_sites Id.initial true in
    let slot, supply = fresh supply "seedless" in
    let unreachable, _ = identity supply "unreachable" in
    let unreachable = { unreachable with environment = [ slot ] } in
    let unreachable =
      { unreachable with
        body =
          Sequence
            ( Function { code = unreachable.code; captures = [ Var slot ] }
            , Return (Function { code; captures = [ Var slot ] }) )
      }
    in
    let source = { source with functions = unreachable :: source.functions } in
    validate name source;
    let program = Known_functions.optimize source in
    validate name program;
    let _ = equal name "42" (observe name Anf_eval.lifted program) in
    let definition =
      List.find
        (fun (definition : Lifted.function_definition) -> Id.equal definition.code code)
        program.functions
    in
    definition.environment <> []
    || fail name "An unresolved incoming capture must make the shared slot unknown")
;;

let generated_helpers =
  QCheck.Test.make
    ~name:
      "generated closed helper chains retain neither environments nor closure allocations"
    ~count:200
    QCheck.(pair (int_range 0 30) int_small)
    (fun (depth, input) ->
       let source =
         List.fold_right
           (fun index body ->
              Printf.sprintf "let f%d x = f%d (x + 1) in %s" index (index - 1) body)
           (List.init depth (fun index -> index + 1))
           (Printf.sprintf "f%d (%d)" depth input)
       in
       let source = "let f0 x = x in " ^ source in
       let program = compile source in
       let _ =
         equal
           source
           (string_of_int (input + depth))
           (observe source Anf_eval.lifted program)
       in
       no_value_closures program
       || fail source "A closed known helper must not remain in any value environment")
;;

let generated_open_helpers =
  QCheck.Test.make
    ~name:"generated helper chains transitively retain a factory's per-call environment"
    ~count:200
    QCheck.(triple (int_range 1 40) int_small int_small)
    (fun (depth, captured, argument) ->
       let source =
         List.fold_right
           (fun index body ->
              Printf.sprintf "let f%d y = f%d y in %s" index (index - 1) body)
           (List.init depth (fun index -> index + 1))
           (Printf.sprintf "f%d" depth)
       in
       let source =
         Printf.sprintf
           "let make x = let f0 y = x + y in %s in let first = make (%d) in let second = \
            make (%d) in first (%d) + second (%d)"
           source
           captured
           (captured + 1)
           argument
           argument
       in
       let program = compile source in
       let _ =
         equal
           source
           (string_of_int (captured + captured + 1 + argument + argument))
           (observe source Anf_eval.lifted program)
       in
       retains_environment program
       || fail source "Transitive calls cannot discard the environment supplying x")
;;

let generated_capture_paths =
  QCheck.Test.make
    ~name:"known code propagates through a generated chain of capture slots"
    ~count:200
    QCheck.(int_range 1 30)
    (fun depth ->
       let indices = List.init (depth + 1) Fun.id in
       let body =
         List.fold_right
           (fun index body ->
              Printf.sprintf "fun x%d -> let _ = x%d + 0 in (%s)" index index body)
           indices
           (Printf.sprintf "inc x%d" depth)
       in
       let calls =
         List.fold_right
           (fun index body ->
              Printf.sprintf
                "let f%d = f%d %d in %s"
                (index + 1)
                index
                (if index = depth then 41 else 0)
                body)
           indices
           (Printf.sprintf "f%d" (depth + 1))
       in
       let source =
         Printf.sprintf "let inc x = x + 1 in let f0 = (%s) in %s" body calls
       in
       let program = compile source in
       let _ = equal source "42" (observe source Anf_eval.lifted program) in
       (environments_empty program && (counts program).direct > 0)
       || fail source "The known closed inc should not remain in any capture slot")
;;

let () =
  run_tests
    (examples
     @ invariants
     @ [ sites "all construction sites agree on the captured code" true
       ; sites "conflicting construction sites prevent specialization" false
       ; unknown_cycle
       ; generated_helpers
       ; generated_open_helpers
       ; generated_capture_paths
       ])
;;
