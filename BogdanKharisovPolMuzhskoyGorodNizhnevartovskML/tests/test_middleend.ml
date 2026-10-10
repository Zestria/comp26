[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend
open Middleend
open Middleend_test_support

let compile source =
  match Pipeline.compile_expression (parsed Parser.parse_expression source) with
  | Ok program -> program
  | Error error -> fail source (Pipeline.error_to_string error)
;;

let behavior name source expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    equal source expected (observe source Anf_eval.lifted (compile source)))
;;

let pair =
  "let rec pair = ((fun x -> x + 1), (fun x -> let (f, _) = pair in f x)) in let (_, g) \
   = pair in g 41"
;;

let behaviors =
  [ behavior
      "full pipeline: two closures from one definition capture different values"
      "let make x = fun y -> x + y in let a = make 10 in let b = make 100 in a 1 + b 1"
      "112"
  ; behavior
      "full pipeline: shadowing does not change a capture"
      "let x = 10 in let f y = x + y in let x = 100 in f 1"
      "11"
  ; behavior
      "full pipeline: captures pass through intermediate closures"
      "let make x = fun y -> fun z -> x + y + z in let f = make 10 in let g = f 20 in g \
       12"
      "42"
  ; behavior
      "full pipeline: tuple parameters and function values in tuples"
      "let make (a, b) = ((fun x -> a + x), (fun x -> b + x)) in let (f, g) = make (10, \
       20) in f 5 + g 7"
      "42"
  ; behavior "full pipeline: recursive pair" pair "42"
  ; behavior
      "full pipeline: recursive tuple with a scalar field"
      "let rec p = (42, fun () -> let (n, _) = p in n) in let (_, f) = p in f ()"
      "42"
  ; behavior
      "full pipeline: recursive function returned through aliases"
      "let rec f = let g = fun n -> if n = 0 then 42 else f (n - 1) in g in f 3"
      "42"
  ; behavior
      "full pipeline: recursive closures from separate factory calls"
      "let make x = let rec f n = if n = 0 then x else f (n - 1) in f in let f = make 10 \
       in let g = make 20 in f 2 + g 3"
      "30"
  ; behavior
      "full pipeline: an unused dynamic recursive initializer is eager"
      "let rec x = 1 / 0 in 42"
      "Division_by_zero"
  ; behavior
      "full pipeline: an independent dynamic recursive initializer"
      "let rec x = 40 + 2 in x"
      "42"
  ; behavior
      "full pipeline: a discarded forward address is not inspected"
      "let rec x = let _ = x in 42 in x"
      "42"
  ; behavior
      "full pipeline: lifting does not evaluate an unused function body"
      "let unused () = 1 / 0 in 42"
      "42"
  ; behavior
      "full pipeline: function creation in an unused branch stays unused"
      "if true then 42 else (fun () -> 1 / 0) ()"
      "42"
  ; behavior
      "full pipeline: partial application retains all supplied arguments"
      "let f x y z = x + y + z in let g = f 10 in let h = g 20 in h 12"
      "42"
  ; behavior
      "full pipeline: overapplication continues with the returned function"
      "let f x = fun y -> x + y in f 20 22"
      "42"
  ; behavior
      "full pipeline: partial application does not execute the function body"
      "(fun x y -> x / y) 1"
      "<function>"
  ; behavior
      "full pipeline: nested functions keep their evaluation boundary"
      "(fun x -> let n = x / 0 in fun y -> n + y) 1"
      "Division_by_zero"
  ; behavior
      "full pipeline: primitive functions can be passed as values"
      "(fun f -> f 20 22) (+)"
      "42"
  ; behavior
      "full pipeline: a rebound operator is captured as a value"
      "let make op = let (+) = op in fun x -> x + 1 in let f = make (fun a b -> a - b) \
       in f 7"
      "6"
  ; behavior
      "full pipeline: direct logical operators keep short circuit"
      "false && (1 / 0 = 0)"
      "false"
  ; behavior
      "full pipeline: captured logical operators are ordinary functions"
      "let make f = fun x -> f false x in let f = make (&&) in f (1 / 0 = 0)"
      "Division_by_zero"
  ; behavior "full pipeline: factorial base case" (cps_factorial 0) "1"
  ; behavior "full pipeline: factorial first recursive step" (cps_factorial 1) "1"
  ; behavior "full pipeline: CPS factorial" (cps_factorial 10) "3628800"
  ; behavior "full pipeline: fibonacci zero base case" (cps_fibonacci 0) "0"
  ; behavior "full pipeline: fibonacci one base case" (cps_fibonacci 1) "1"
  ; behavior "full pipeline: CPS fibonacci" (cps_fibonacci 10) "55"
  ]
;;

(* Exercise builtin resolution and execution through the complete pipeline.
   The existing arithmetic/short-circuit tests cover the other primitives. *)
let builtin_operations =
  [ behavior "mod: positive remainder" "7 mod 3" "1"
  ; behavior "mod: negative dividend" "(-7) mod 3" "-1"
  ; behavior "mod: negative divisor" "7 mod (-3)" "1"
  ; behavior "mod: both operands negative" "(-7) mod (-3)" "-1"
  ; behavior "mod: zero dividend" "0 mod 3" "0"
  ; behavior "mod: zero divisor raises" "7 mod 0" "Division_by_zero"
  ; behavior "mod: exact multiple" "6 mod 3" "0"
  ; behavior "mod: dividend smaller than divisor" "2 mod 3" "2"
  ; behavior "mod: unit divisor" "7 mod 1" "0"
  ; behavior "mod: negative unit divisor" "7 mod (-1)" "0"
  ; behavior "not: true becomes false" "not true" "false"
  ; behavior "not: false becomes true" "not false" "true"
  ; behavior "not equal: smaller operands" "(-1) <> 0" "true"
  ; behavior "not equal: equal operands" "0 <> 0" "false"
  ; behavior "not equal: greater operands" "1 <> 0" "true"
  ; behavior "less or equal: smaller operands" "(-1) <= 0" "true"
  ; behavior "less or equal: equal operands" "0 <= 0" "true"
  ; behavior "less or equal: greater operands" "1 <= 0" "false"
  ; behavior "greater: smaller operands" "(-1) > 0" "false"
  ; behavior "greater: equal operands" "0 > 0" "false"
  ; behavior "greater: greater operands" "1 > 0" "true"
  ; behavior "greater or equal: smaller operands" "(-1) >= 0" "false"
  ; behavior "greater or equal: equal operands" "0 >= 0" "true"
  ; behavior "greater or equal: greater operands" "1 >= 0" "true"
  ; behavior "negate: positive variable" "(fun x -> -x) 42" "-42"
  ; behavior "negate: negative variable" "(fun x -> -x) (-42)" "42"
  ; behavior "negate: zero variable" "(fun x -> -x) 0" "0"
  ]
;;

let generated_remainders =
  let inputs =
    let open QCheck.Gen in
    let ( let* ) = ( >>= ) in
    let* divisor = int_range 1 1000 in
    let* quotient = int_range 0 1000 in
    let* remainder = int_range 0 (divisor - 1) in
    let* negative_dividend = bool in
    let* negative_divisor = bool in
    let dividend = (quotient * divisor) + remainder in
    let dividend, expected =
      if negative_dividend then -dividend, -remainder else dividend, remainder
    in
    let divisor = if negative_divisor then -divisor else divisor in
    return (dividend, divisor, expected)
  in
  QCheck.Test.make
    ~name:"generated remainder agrees with the constructed quotient and residue"
    ~count:1000
    (QCheck.make ~print:QCheck.Print.(triple int int int) inputs)
    (fun (dividend, divisor, expected) ->
       (* Construct n = q*d+r with 0 <= r < d: no implementation of mod
          or call to it is used to calculate the expected result. *)
       let source =
         Printf.sprintf
           "let remainder n d = n mod d in remainder (%d) (%d)"
           dividend
           divisor
       in
       equal
         source
         (string_of_int expected)
         (observe source Anf_eval.lifted (compile source)))
;;

let generated_order =
  QCheck.Test.make
    ~name:"generated ordered integers retain strict and non-strict relations"
    ~count:1000
    QCheck.(pair (int_range (-1000000) 1000000) (int_range 1 1000))
    (fun (lower, gap) ->
       let source =
         Printf.sprintf
           "let lower = (%d) in let upper = (%d) in lower <> upper && lower <= upper && \
            upper > lower && upper >= lower"
           lower
           (lower + gap)
       in
       equal source "true" (observe source Anf_eval.lifted (compile source)))
;;

let close source =
  let lambda, supply = lower_with_supply Id.initial source in
  let anf, supply = allocated source (Anf.of_lambda supply lambda) in
  fst (allocated source (Closure_conversion.convert supply anf))
;;

let closure_stage name source expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let closed = close source in
    (match Closure.validate closed with
     | Ok () -> ()
     | Error error -> fail source (Closure.error_to_string error));
    equal source expected (observe source Anf_eval.closed closed))
;;

let minimal_environment =
  let source = "fun x unused -> fun y -> x + y" in
  QCheck.Test.make
    ~name:"closure conversion captures exactly the used binding"
    ~count:1
    QCheck.unit
    (fun () ->
       match close source with
       | Return
           (Function
              { environment = []
              ; captures = []
              ; parameter = x
              ; body =
                  Return
                    (Function { environment = [ slot ]; captures = [ Var captured ]; _ })
              ; _
              })
         when Id.equal x captured && not (Id.equal slot captured) -> true
       | _ ->
         fail
           source
           "Expected one fresh environment slot for x, no capture of unused or (+)")
;;

let all_lifted =
  QCheck.Test.make
    ~name:"lifting moves every CPS fibonacci function into one code declaration"
    ~count:1
    QCheck.unit
    (fun () ->
       let program = compile (cps_fibonacci 10) in
       equal "CPS fibonacci" "4" (string_of_int (List.length program.functions)))
;;

let closed_scope =
  QCheck.Test.make
    ~name:"closed code cannot implicitly use its construction scope"
    ~count:1
    QCheck.unit
    (fun () ->
       let outer, supply = fresh Id.initial "outer" in
       let param, _ = fresh supply "arg" in
       let closure : Closure.t =
         { environment = []
         ; captures = []
         ; parameter = param
         ; parameters = []
         ; body = Return (Atom (Var outer))
         }
       in
       let term = Anf.Let (outer, Atom (Constant Unit), Return (Function closure)) in
       match Closure.validate term with
       | Error (Binding (Unbound_variable id)) when Id.equal id outer -> true
       | _ ->
         fail
           "let outer = () in closure [] (fun arg -> outer)"
           "Expected rejection of the implicit capture")
;;

let capture_count =
  QCheck.Test.make
    ~name:"closure environments and captures must have the same size"
    ~count:1
    QCheck.unit
    (fun () ->
       let slot, supply = fresh Id.initial "slot" in
       let param, _ = fresh supply "arg" in
       let closure : Closure.t =
         { environment = [ slot ]
         ; captures = []
         ; parameter = param
         ; parameters = []
         ; body = Return (Atom (Var slot))
         }
       in
       match Closure.validate (Return (Function closure)) with
       | Error (Capture_count (1, 0)) -> true
       | _ -> fail "one slot, no captured value" "Expected a capture-count error")
;;

let code_definition supply name =
  let code, supply = fresh supply name in
  let parameter, supply = fresh supply "arg" in
  ( { Lifted.code
    ; environment = []
    ; parameter
    ; parameters = []
    ; body = Return (Atom (Var parameter))
    }
  , supply )
;;

let invalid_lifted name construct check =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    match Lifted.validate (construct ()) with
    | Error error when check error -> true
    | Error error -> fail name ("Wrong invariant failure: " ^ Lifted.error_to_string error)
    | Ok () -> fail name "Expected an invariant failure")
;;

let invariants =
  [ invalid_lifted
      "a closure cannot refer to absent code"
      (fun () ->
         let code, _ = fresh Id.initial "missing" in
         { Lifted.functions = []; body = Return (Function { code; captures = [] }) })
      (function
        | Unknown_code _ -> true
        | _ -> false)
  ; invalid_lifted
      "code labels must be unique"
      (fun () ->
         let first, supply = code_definition Id.initial "duplicate" in
         let second, _ = code_definition supply "second" in
         let second = { second with code = first.code } in
         { Lifted.functions = [ first; second ]; body = Return (Atom (Constant Unit)) })
      (function
        | Duplicate_code _ -> true
        | _ -> false)
  ; invalid_lifted
      "lifted code cannot use an entry-point binding implicitly"
      (fun () ->
         let outer, supply = fresh Id.initial "outer" in
         let definition, _ = code_definition supply "code" in
         let definition = { definition with body = Return (Atom (Var outer)) } in
         { Lifted.functions = [ definition ]
         ; body =
             Let
               ( outer
               , Atom (Constant Unit)
               , Return (Function { code = definition.code; captures = [] }) )
         })
      (function
        | Binding (Unbound_variable _) -> true
        | _ -> false)
  ; invalid_lifted
      "a lifted closure must supply all environment slots"
      (fun () ->
         let definition, supply = code_definition Id.initial "code" in
         let capture, _ = fresh supply "capture" in
         let definition = { definition with environment = [ capture ] } in
         { Lifted.functions = [ definition ]
         ; body = Return (Function { code = definition.code; captures = [] })
         })
      (function
        | Capture_count (_, 1, 0) -> true
        | _ -> false)
  ]
;;

let closed_invariant name construct expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let term, id = construct () in
    match Closure.validate term with
    | Error (Binding error) when error = expected id -> true
    | Error error ->
      fail name ("Wrong invariant failure: " ^ Closure.error_to_string error)
    | Ok () -> fail name "Expected closure scope validation to fail")
;;

let empty_closure parameter : Closure.t =
  { environment = []
  ; captures = []
  ; parameter
  ; parameters = []
  ; body = Return (Atom (Constant Unit))
  }
;;

let closure_invariants =
  [ closed_invariant
      "a closure parameter cannot reuse an environment slot"
      (fun () ->
         let id, _ = fresh Id.initial "slot_and_parameter" in
         ( Anf.Return
             (Function
                { (empty_closure id) with
                  environment = [ id ]
                ; captures = [ Constant Unit ]
                })
         , id ))
      (fun id -> Lambda.Duplicate_binding id)
  ; closed_invariant
      "different closures cannot share a parameter identity"
      (fun () ->
         let id, _ = fresh Id.initial "shared_parameter" in
         let function_ = Anf.Function (empty_closure id) in
         Anf.Sequence (function_, Return function_), id)
      (fun id -> Lambda.Duplicate_binding id)
  ; closed_invariant
      "explicit closure captures must exist at construction"
      (fun () ->
         let id, supply = fresh Id.initial "missing_capture" in
         let arg, supply = fresh supply "arg" in
         let slot, _ = fresh supply "slot" in
         ( Anf.Return
             (Function
                { (empty_closure arg) with environment = [ slot ]; captures = [ Var id ] })
         , id ))
      (fun id -> Lambda.Unbound_variable id)
  ; closed_invariant
      "environment slots must be distinct within a closure"
      (fun () ->
         let id, supply = fresh Id.initial "repeated_slot" in
         let arg, _ = fresh supply "arg" in
         ( Anf.Return
             (Function
                { (empty_closure arg) with
                  environment = [ id; id ]
                ; captures = [ Constant Unit; Constant Unit ]
                })
         , id ))
      (fun id -> Lambda.Duplicate_binding id)
  ]
;;

let lifted_binding_collision name construct =
  invalid_lifted name construct (function
    | Binding (Duplicate_binding _) -> true
    | _ -> false)
;;

let lifted_boundaries =
  [ lifted_binding_collision
      "lifted code declarations cannot share parameter identities"
      (fun () ->
         let first, supply = code_definition Id.initial "first" in
         let code, _ = fresh supply "second" in
         let second = { first with code } in
         { Lifted.functions = [ first; second ]; body = Return (Atom (Constant Unit)) })
  ; lifted_binding_collision
      "lifted code declarations cannot share environment slot identities"
      (fun () ->
         let slot, supply = fresh Id.initial "shared_slot" in
         let first, supply = code_definition supply "first" in
         let second, _ = code_definition supply "second" in
         { Lifted.functions =
             [ { first with environment = [ slot ] }
             ; { second with environment = [ slot ] }
             ]
         ; body = Return (Atom (Constant Unit))
         })
  ; lifted_binding_collision
      "lifted parameters and environment slots cannot collide"
      (fun () ->
         let definition, _ = code_definition Id.initial "code" in
         { Lifted.functions =
             [ { definition with environment = [ definition.parameter ] } ]
         ; body = Return (Atom (Constant Unit))
         })
  ; lifted_binding_collision
      "entry bindings cannot reuse lifted function parameters"
      (fun () ->
         let definition, _ = code_definition Id.initial "code" in
         { Lifted.functions = [ definition ]
         ; body =
             Let
               (definition.parameter, Atom (Constant Unit), Return (Atom (Constant Unit)))
         })
  ; invalid_lifted
      "a lifted capture cannot use an unbound construction-time value"
      (fun () ->
         let definition, supply = code_definition Id.initial "code" in
         let slot, supply = fresh supply "slot" in
         let missing, _ = fresh supply "missing" in
         let definition = { definition with environment = [ slot ] } in
         { Lifted.functions = [ definition ]
         ; body = Return (Function { code = definition.code; captures = [ Var missing ] })
         })
      (function
        | Binding (Unbound_variable _) -> true
        | _ -> false)
  ; invalid_lifted
      "a lifted closure cannot supply extra environment values"
      (fun () ->
         let definition, _ = code_definition Id.initial "code" in
         { Lifted.functions = [ definition ]
         ; body =
             Return (Function { code = definition.code; captures = [ Constant Unit ] })
         })
      (function
        | Capture_count (_, 0, 1) -> true
        | _ -> false)
  ]
;;

let structural_comparisons =
  [ behavior
      "runtime equality compares independently allocated tuple contents"
      "(40 + 2, (true, ())) = (42, (true, ()))"
      "true"
  ; behavior
      "runtime tuple comparison stops before a later function field"
      "(1, fun x -> x) < (2, fun x -> x)"
      "true"
  ; QCheck.Test.make
      ~name:"runtime comparison rejects an inspected function field"
      ~count:1
      QCheck.unit
      (fun () ->
         let source = "(1, fun x -> x) = (1, fun x -> x)" in
         match Anf_eval.lifted (compile source) with
         | Error (Eval_value.Runtime Functional_comparison) -> true
         | Error error ->
           fail source ("Unexpected error: " ^ Eval_value.error_to_string error)
         | Ok value ->
           fail source ("Expected functional comparison to fail, found " ^ value))
  ]
;;

let diagnostic_source =
  QCheck.Test.make
    ~name:"unexpected evaluator exceptions retain source and the original backtrace"
    ~count:1
    QCheck.unit
    (fun () ->
       let source = "a failing interpreted program" in
       let probe =
         QCheck.Test.make_cell ~count:1 QCheck.unit (fun () ->
           let _ = observe source (fun () -> raise Not_found) () in
           true)
       in
       match QCheck.TestResult.get_state (QCheck.Test.check_cell probe) with
       | Failed { instances = [ { msg_l = [ message ]; _ } ] } ->
         (match String.split_on_char '\n' message with
          | input :: error :: backtrace
            when input = "Input: " ^ source
                 && error = "Unexpected evaluation exception: Not_found"
                 && List.exists (String.starts_with ~prefix:"Raised at ") backtrace ->
            true
          | _ -> fail source ("Incomplete diagnostic: " ^ message))
       | _ ->
         fail source "Expected an explicit failure report from the diagnostic wrapper")
;;

let factories =
  QCheck.Test.make
    ~name:"generated closure factories preserve per-call environments through every pass"
    ~count:1000
    QCheck.(triple int_small int_small int_small)
    (fun (a, b, c) ->
       let source =
         Printf.sprintf
           "let make x = fun y -> x + y in let f = make (%d) in let g = make (%d) in f \
            (%d) + g (%d)"
           a
           b
           c
           c
       in
       equal
         source
         (string_of_int (a + b + c + c))
         (observe source Anf_eval.lifted (compile source)))
;;

let nested_environments =
  QCheck.Test.make
    ~name:"generated nesting depths and capture sets preserve weighted arguments"
    ~count:1000
    QCheck.(list_size Gen.(1 -- 8) (pair int_small (int_range (-3) 3)))
    (fun inputs ->
       let indexed = List.mapi (fun i input -> i, input) inputs in
       let terms =
         List.filter_map
           (fun (i, (_, weight)) ->
              if weight = 0 then None else Some (Printf.sprintf "x%d * (%d)" i weight))
           indexed
       in
       let body = String.concat " + " ("0" :: terms) in
       let function_ =
         List.fold_right
           (fun (i, _) body -> Printf.sprintf "fun x%d -> (%s)" i body)
           indexed
           body
       in
       let calls =
         List.fold_right
           (fun (i, (argument, _)) body ->
              Printf.sprintf "let f%d = f%d (%d) in %s" (i + 1) i argument body)
           indexed
           (Printf.sprintf "f%d" (List.length inputs))
       in
       let source = "let f0 = (" ^ function_ ^ ") in " ^ calls in
       let expected =
         List.fold_left (fun sum (argument, weight) -> sum + (argument * weight)) 0 inputs
       in
       equal
         source
         (string_of_int expected)
         (observe source Anf_eval.lifted (compile source)))
;;

let stable_output =
  QCheck.Test.make
    ~name:"full output does not depend on prior identifier allocation"
    ~count:100
    QCheck.int_small
    (fun n ->
       let source = Printf.sprintf "let f x = fun y -> x + y in f (%d) 2" n in
       let first = Format.asprintf "%a" Lifted_print.program (compile source) in
       let _ = compile (cps_factorial 3) in
       equal source first (Format.asprintf "%a" Lifted_print.program (compile source)))
;;

let continued_supplies =
  let source = "let make x = fun y -> x + (y + 1) in let f = make 40 in f 1" in
  let normalized () =
    let lambda, supply = lower_with_supply Id.initial source in
    allocated source (Anf.of_lambda supply lambda)
  in
  let closed () =
    let anf, supply = normalized () in
    allocated source (Closure_conversion.convert supply anf)
  in
  let lifted () =
    let closed, supply = closed () in
    allocated source (Lambda_lifting.lift supply closed)
  in
  let check stage collect build =
    QCheck.Test.make
      ~name:(stage ^ " returns a supply beyond its output identities")
      ~count:1
      QCheck.unit
      (fun () ->
         let term, supply = build () in
         let next, _ = fresh supply "next" in
         if List.exists (Id.equal next) (collect [] term)
         then fail source (stage ^ " returned a supply that reuses an output identity");
         true)
  in
  let rec anf_ids ids expression =
    Anf.fold
      ~atom:(fun ids _ -> ids)
      ~binder:(fun ids id -> id :: ids)
      ~function_:(fun ids (definition : Anf.function_definition) ->
        anf_ids ((definition.parameter :: definition.parameters) @ ids) definition.body)
      ids
      expression
  in
  let rec closed_ids ids expression =
    Anf.fold
      ~atom:(fun ids _ -> ids)
      ~binder:(fun ids id -> id :: ids)
      ~function_:(fun ids (closure : Closure.t) ->
        closed_ids
          (closure.environment @ (closure.parameter :: closure.parameters) @ ids)
          closure.body)
      ids
      expression
  in
  let lifted_ids ids (program : Lifted.program) =
    let term_ids =
      Anf.fold
        ~atom:(fun ids _ -> ids)
        ~binder:(fun ids id -> id :: ids)
        ~function_:(fun ids _ -> ids)
    in
    List.fold_left
      (fun ids (definition : Lifted.function_definition) ->
         term_ids
           ((definition.code :: definition.environment)
            @ (definition.parameter :: definition.parameters)
            @ ids)
           definition.body)
      (term_ids ids program.body)
      program.functions
  in
  [ check "ANF" anf_ids normalized
  ; check "CC" closed_ids closed
  ; check "LL" lifted_ids lifted
  ]
;;

let interpreter_error name build accepts expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    match build () with
    | Error error when accepts error -> true
    | Error error ->
      fail name ("Expected " ^ expected ^ ", got " ^ Eval_value.error_to_string error)
    | Ok value -> fail name ("Expected " ^ expected ^ ", got value " ^ value))
;;

let interpreter_errors =
  [ interpreter_error
      "a closure interpreter rejects a missing capture"
      (fun () ->
         let slot, supply = fresh Id.initial "slot" in
         let arg, _ = fresh supply "arg" in
         let closure : Closure.t =
           { environment = [ slot ]
           ; captures = []
           ; parameter = arg
           ; parameters = []
           ; body = Return (Atom (Var slot))
           }
         in
         Anf_eval.closed (Return (Function closure)))
      (function
        | Eval_value.Invalid_ir (Capture_arity (1, 0)) -> true
        | _ -> false)
      "a capture-arity error"
  ; interpreter_error
      "a closure interpreter rejects an extra capture"
      (fun () ->
         let arg, _ = fresh Id.initial "arg" in
         let closure : Closure.t =
           { environment = []
           ; captures = [ Constant Unit ]
           ; parameter = arg
           ; parameters = []
           ; body = Return (Atom (Var arg))
           }
         in
         Anf_eval.closed (Return (Function closure)))
      (function
        | Eval_value.Invalid_ir (Capture_arity (0, 1)) -> true
        | _ -> false)
      "a capture-arity error"
  ; interpreter_error
      "a lifted interpreter reports absent closure code"
      (fun () ->
         let code, _ = fresh Id.initial "missing" in
         Anf_eval.lifted
           { functions = []; body = Return (Function { code; captures = [] }) })
      (function
        | Eval_value.Invalid_ir (Unknown_code id) -> Id.name id = "missing"
        | _ -> false)
      "an unknown-code error"
  ]
;;

let () =
  run_tests
    (behaviors
     @ builtin_operations
     @ invariants
     @ interpreter_errors
     @ closure_invariants
     @ lifted_boundaries
     @ structural_comparisons
     @ continued_supplies
     @ [ closure_stage "closure conversion alone: cyclic pair" pair "42"
       ; closure_stage
           "closure conversion alone: captures through nested functions"
           "(fun x -> fun y -> fun z -> x + y + z) 10 20 12"
           "42"
       ; minimal_environment
       ; all_lifted
       ; closed_scope
       ; capture_count
       ; factories
       ; nested_environments
       ; generated_remainders
       ; generated_order
       ; stable_output
       ; diagnostic_source
       ])
;;
