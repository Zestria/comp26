[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_frontend
open Bkml_middleend
open Bkml_middleend_test_support

let example name source expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    equal source expected (observe source Lambda_eval.observe (lower source)))
;;

let rejects name source expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    match Lower.expression Id.initial (parsed Parser.parse_expression source) with
    | Error error -> equal source expected (Lower.error_to_string error)
    | Ok _ -> fail source ("Expected rejection: " ^ expected))
;;

let factorial = cps_factorial 6
let fibonacci = cps_fibonacci 10

let examples =
  [ example
      "nonrecursive rhs sees the preceding binding"
      "let x = 10 in let x = x + 1 in x"
      "11"
  ; example
      "a closure retains the captured definition"
      "let x = 10 in let f y = x + y in let x = 100 in f 1"
      "11"
  ; example
      "different calls have different captured values"
      "let make x = fun y -> x + y in let a = make 10 in let b = make 100 in a 1 + b 1"
      "112"
  ; example "nested scope does not escape" "let x = 10 in (let x = 20 in x) + x" "30"
  ; example "parameters may shadow preceding parameters" "(fun x x -> x) 1 2" "2"
  ; example
      "nested tuple and unit patterns"
      "let ((a, _), ((), b)) = ((10, 999), ((), 2)) in a + b"
      "12"
  ; example
      "tuple parameters and partial application"
      "let f (x, y) () z = x + y + z in let g = f (10, 20) in g () 3"
      "33"
  ; example
      "generated tuple bindings cannot capture source names"
      "let tuple = 40 in let (x, y) = (1, 2) in tuple + x + y"
      "43"
  ; example
      "generated parameters cannot capture source names"
      "let arg = 40 in (fun (x, y) -> arg + x + y) (1, 2)"
      "43"
  ; example "wildcard rhs is still evaluated" "let _ = 1 / 0 in 42" "Division_by_zero"
  ; example "missing else returns unit" "if false then let _ = 1 / 0 in ()" "()"
  ; example "short circuit and skips rhs" "false && (1 / 0 = 0)" "false"
  ; example
      "short circuit and evaluates required rhs"
      "true && (1 / 0 = 0)"
      "Division_by_zero"
  ; example "short circuit or skips rhs" "true || (1 / 0 = 0)" "true"
  ; example
      "short circuit or evaluates required rhs"
      "false || (1 / 0 = 0)"
      "Division_by_zero"
  ; example
      "rebound and is strict"
      "let (&&) a b = a in false && (1 / 0 = 0)"
      "Division_by_zero"
  ; example
      "aliased and is strict"
      "let both = (&&) in both false (1 / 0 = 0)"
      "Division_by_zero"
  ; example
      "a separate partial and application is strict"
      "((&&) false) (1 / 0 = 0)"
      "Division_by_zero"
  ; example
      "rebound arithmetic is an ordinary function"
      "let (+) a b = a - b in 7 + 2"
      "5"
  ; example
      "custom operators resolve to their definitions"
      "let (%%) x y = x + y in 3 %% 4"
      "7"
  ; example
      "keyword of an absent language feature remains a name"
      "let class = 42 in class"
      "42"
  ; example
      "an unused recursive marker on a constant is retained"
      "let rec x = 42 in x"
      "42"
  ; example
      "recursion sees itself, not the shadowed definition"
      "let f n = 99 in let rec f n = if n = 0 then 1 else n * f (n - 1) in f 5"
      "120"
  ; example
      "multi-parameter body waits for saturation"
      "(fun x y -> x / y) 1"
      "<function>"
  ; example
      "nested functions retain their evaluation boundary"
      "(fun x -> let n = x / 0 in fun y -> n + y) 1"
      "Division_by_zero"
  ; example
      "largest integer literal is accepted"
      "4611686018427387903"
      "4611686018427387903"
  ; example
      "smallest integer literal is accepted"
      "-4611686018427387904"
      "-4611686018427387904"
  ; example "CPS factorial" factorial "720"
  ; example "CPS fibonacci" fibonacci "55"
  ; rejects "unbound variable" "unknown" "Unbound value unknown"
  ; rejects
      "unused out-of-range integer literal"
      "let _ = 4611686018427387904 in 42"
      "Integer literal exceeds the range of representable integers of type int: \
       4611686018427387904"
  ; rejects
      "positive integer literal above maximum"
      "4611686018427387904"
      "Integer literal exceeds the range of representable integers of type int: \
       4611686018427387904"
  ; rejects
      "negative integer literal below minimum"
      "-4611686018427387905"
      "Integer literal exceeds the range of representable integers of type int: \
       -4611686018427387905"
  ; rejects
      "nonrecursive binding cannot refer to itself"
      "let x = x in x"
      "Unbound value x"
  ; rejects
      "a local binding does not escape its scope"
      "(let x = 1 in x) + x"
      "Unbound value x"
  ; rejects
      "duplicate nested pattern variable"
      "fun (x, (_, x)) -> x"
      "Variable x is bound more than once in this pattern"
  ; rejects
      "recursive tuple pattern"
      "let rec (x, y) = (1, 2) in x"
      "Only variables are allowed on the left of let rec"
  ; rejects
      "recursive wildcard pattern"
      "let rec _ = 1 in 2"
      "Only variables are allowed on the left of let rec"
  ; rejects
      "recursive unit pattern"
      "let rec () = () in ()"
      "Only variables are allowed on the left of let rec"
  ]
;;

let program_case name source expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    match Lower.program Id.initial (parsed Parser.parse_program source) with
    | Error error -> equal source expected (Lower.error_to_string error)
    | Ok (term, _) ->
      equal source expected (observe source Lambda_eval.observe (checked source term)))
;;

let program_cases =
  [ program_case "empty program" "" "()"
  ; program_case
      "top-level closure retains its environment"
      "let x = 10;; let f y = x + y;; let x = 100;; if f 1 = 11 then () else let _ = 1 / \
       0 in ()"
      "()"
  ; program_case
      "top-level definitions are not visible before declaration"
      "let f x = later x;; let later x = x"
      "Unbound value later"
  ; program_case
      "evaluation items do not introduce top-level bindings"
      "(let x = 1 in x);; x"
      "Unbound value x"
  ; program_case
      "evaluation items execute in order"
      "1 / 0;; let x = 42"
      "Division_by_zero"
  ]
;;

let tuple_once =
  let source = "let make x = (x, x) in let (a, b) = make 7 in a + b" in
  QCheck.Test.make
    ~name:"tuple rhs is bound once and projections share it"
    ~count:1
    QCheck.unit
    (fun () ->
       match lower source with
       | Lambda.Let
           ( make
           , Fun _
           , Let
               ( tuple
               , Apply (Var callee, _, [])
               , Let (_, Field (Var first, 0), Let (_, Field (Var second, 1), _)) ) )
         when Id.equal make callee && Id.equal tuple first && Id.equal tuple second ->
         true
       | _ ->
         fail source "Expected one call to make, with both fields read from its result")
;;

let captures =
  QCheck.Test.make
    ~name:"free variables distinguish same-spelled definitions"
    ~count:1
    QCheck.unit
    (fun () ->
       let outer, supply = fresh Id.initial "x" in
       let inner, supply = fresh supply "x" in
       let param, _ = fresh supply "y" in
       let term =
         Lambda.Fun
           { parameter = param
           ; parameters = []
           ; body = Let (inner, Var param, Tuple (Var outer, Var inner, []))
           }
       in
       let free = Lambda.free_variables term in
       if Id.Set.cardinal free <> 1 || not (Id.Set.mem outer free)
       then
         fail
           "fun y -> let x_inner = y in (x_outer, x_inner)"
           "Only the outer x should be free";
       true)
;;

let recursive_free_variables =
  QCheck.Test.make
    ~name:"recursive binding removes self references from rhs and body"
    ~count:1
    QCheck.unit
    (fun () ->
       let f, supply = fresh Id.initial "f" in
       let outer, _ = fresh supply "outer" in
       let term = Lambda.Let_rec (f, Apply (Var f, Var outer, []), Var f) in
       let free = Lambda.free_variables term in
       if Id.Set.cardinal free <> 1 || not (Id.Set.mem outer free)
       then fail "let rec f = f outer in f" "Only outer should be free";
       true)
;;

let invariant name term expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let expression = term () in
    let actual =
      match Lambda.validate expression with
      | Ok () -> "valid"
      | Error error -> invariant_error error
    in
    equal name expected actual)
;;

let invariants =
  [ invariant
      "nonrecursive rhs cannot see its own binder"
      (fun () ->
         let x, _ = fresh Id.initial "x" in
         Lambda.Let (x, Var x, Var x))
      "Unbound Id: x"
  ; invariant
      "a binder cannot be duplicated into two branches"
      (fun () ->
         let x, _ = fresh Id.initial "x" in
         let branch = Lambda.Let (x, Constant Unit, Var x) in
         Lambda.If (Constant (Bool true), branch, branch))
      "Repeated binding Id: x"
  ; invariant
      "a branch binding does not escape into its sibling"
      (fun () ->
         let x, _ = fresh Id.initial "x" in
         Lambda.If (Constant (Bool true), Let (x, Constant Unit, Var x), Var x))
      "Unbound Id: x"
  ; invariant
      "a field index must be nonnegative"
      (fun () -> Lambda.Field (Tuple (Constant Unit, Constant Unit, []), -1))
      "Negative field: -1"
  ]
;;

let shadowing_property =
  QCheck.Test.make
    ~name:"generated closures preserve the captured binding under shadowing"
    ~count:1000
    QCheck.(triple int_small int_small int_small)
    (fun (captured, shadow, argument) ->
       let source =
         Printf.sprintf
           "let x = %d in let f y = x + y in let x = %d in f (%d)"
           captured
           shadow
           argument
       in
       equal
         source
         (string_of_int (captured + argument))
         (observe source Lambda_eval.observe (lower source)))
;;

let ids_property =
  QCheck.Test.make
    ~name:"fresh and renamed identities coexist as distinct map keys"
    ~count:1000
    QCheck.string
    (fun name ->
       let first, supply = fresh Id.initial name in
       let second, supply = fresh supply name in
       let renamed, _ = rename supply first in
       let bindings = Id.Map.(empty |> add first 1 |> add second 2 |> add renamed 3) in
       if
         Id.Map.cardinal bindings <> 3
         || Id.Map.find first bindings <> 1
         || Id.Map.find second bindings <> 2
         || Id.Map.find renamed bindings <> 3
         || Id.name renamed <> name
       then
         fail
           (Printf.sprintf "name = %S" name)
           "Fresh identities collided or rename lost the name";
       true)
;;

let composed_lowering =
  let first_source = "let x = 10 in fun () -> x" in
  let second_source = "let x = 20 in fun () -> x" in
  let source =
    Printf.sprintf
      "let pair = ((%s), (%s)) in (field[0] pair (), field[1] pair ())"
      first_source
      second_source
  in
  QCheck.Test.make
    ~name:"separately lowered closures retain their captures when composed"
    ~count:1
    QCheck.unit
    (fun () ->
       let first, supply = lower_with_supply Id.initial first_source in
       let second, supply = lower_with_supply supply second_source in
       let pair, _ = fresh supply "pair" in
       let term =
         Lambda.Let
           ( pair
           , Tuple (first, second, [])
           , Tuple
               ( Apply (Field (Var pair, 0), Constant Unit, [])
               , Apply (Field (Var pair, 1), Constant Unit, [])
               , [] ) )
       in
       let term = checked source term in
       equal source "(10, 20)" (observe source Lambda_eval.observe term))
;;

let menhir_cps =
  QCheck.Test.make
    ~name:"Menhir CPS output integrates with lowering"
    ~count:1
    QCheck.unit
    (fun () ->
       match
         Lower.expression Id.initial (parsed Menhir_parser.parse_expression fibonacci)
       with
       | Error error -> fail fibonacci (Lower.error_to_string error)
       | Ok (term, _) ->
         equal
           fibonacci
           "55"
           (observe fibonacci Lambda_eval.observe (checked fibonacci term)))
;;

let () =
  run_tests
    (examples
     @ program_cases
     @ invariants
     @ [ tuple_once
       ; captures
       ; recursive_free_variables
       ; shadowing_property
       ; ids_property
       ; composed_lowering
       ; menhir_cps
       ])
;;
