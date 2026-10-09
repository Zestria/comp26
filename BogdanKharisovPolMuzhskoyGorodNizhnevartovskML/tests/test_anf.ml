[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Middleend
open Middleend_test_support

let normalize_with_supply supply source =
  let lambda, supply = lower_with_supply supply source in
  let result, supply = allocated source (Anf.of_lambda supply lambda) in
  (match Anf.validate result with
   | Ok () -> ()
   | Error error -> fail source (invariant_error error));
  result, supply
;;

let normalize source = fst (normalize_with_supply Id.initial source)

let behavior name source expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    equal
      source
      expected
      (observe source Lambda_eval.observe (Anf.to_lambda (normalize source))))
;;

let shape name source predicate expectation =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let result = normalize source in
    if not (predicate result)
    then
      fail
        source
        (expectation
         ^ "\nActual ANF:\n"
         ^ Format.asprintf "%a" Anf_print.expression result);
    true)
;;

let behaviors =
  [ behavior
      "ANF preserves effects in a discarded expression"
      "let _ = 1 / 0 in 42"
      "Division_by_zero"
  ; behavior
      "ANF keeps the unused conditional branch unevaluated"
      "(if true then 40 else 1 / 0) + 2"
      "42"
  ; behavior
      "ANF keeps short circuit rhs inside its branch"
      "false && (1 / 0 = 0)"
      "false"
  ; behavior
      "ANF does not turn an aliased logical function into short circuit"
      "let both = (&&) in both false (1 / 0 = 0)"
      "Division_by_zero"
  ; behavior
      "ANF does not erase an effectful non-atomic binding"
      "let x = 1 / 0 in 42"
      "Division_by_zero"
  ; behavior
      "ANF aliases respect lexical identity"
      "let x = 10 in let f y = x + y in let x = 100 in f 1"
      "11"
  ; behavior
      "ANF normalizes tuple projections and their uses"
      "let (a, (b, c)) = (1 + 2, (3 + 4, 5 + 6)) in a + b + c"
      "21"
  ; behavior
      "ANF retains recursive constant initialization"
      "let rec x = 42 in x + 1"
      "43"
  ; behavior
      "ANF preserves nested function application boundaries"
      "(fun x -> let n = x / 0 in fun y -> n + y) 1"
      "Division_by_zero"
  ; behavior
      "ANF preserves partial application"
      "let add x y = x + y in let add10 = add 10 in add10 2"
      "12"
  ; behavior "ANF CPS factorial" (cps_factorial 7) "5040"
  ; behavior "ANF CPS factorial base case" (cps_factorial 0) "1"
  ; behavior "ANF CPS factorial first recursive step" (cps_factorial 1) "1"
  ; behavior "ANF CPS fibonacci" (cps_fibonacci 10) "55"
  ; behavior "ANF CPS fibonacci zero base case" (cps_fibonacci 0) "0"
  ; behavior "ANF CPS fibonacci one base case" (cps_fibonacci 1) "1"
  ]
;;

let shapes =
  [ shape
      "ANF eliminates chains of atomic aliases"
      "let x = 42 in let y = x in y"
      (function
        | Return (Atom (Constant (Int n))) -> Frontend.Integer_literal.to_string n = "42"
        | _ -> false)
      "Expected the constant itself, without any bindings"
  ; shape
      "ANF returns a tail computation without a result temporary"
      "let x = 1 + 2 in x"
      (function
        | Return (Apply (Primitive Add, Constant _, [ Constant _ ])) -> true
        | _ -> false)
      "Expected one direct addition in return position"
  ; shape
      "ANF keeps a saturated primitive application together"
      "1 + 2"
      (function
        | Return (Apply (Primitive Add, _, [ _ ])) -> true
        | _ -> false)
      "Expected one application with two arguments"
  ; shape
      "ANF keeps explicitly separate applications separate"
      "((+) 1) 2"
      (function
        | Let (partial, Apply (Primitive Add, _, []), Return (Apply (Var callee, _, [])))
          -> Id.equal partial callee
        | _ -> false)
      "Expected a partial application followed by another call"
  ; shape
      "ANF evaluates arguments before the callee expression"
      "(if 1 = 1 then fun x -> x else fun x -> x) (2 + 3)"
      (function
        | Let (_, Apply (Primitive Add, _, _), Let (_, Apply (Primitive Equal, _, _), _))
          -> true
        | _ -> false)
      "Expected addition before computation of the function expression"
  ; shape
      "ANF evaluates multi-argument operands right to left"
      "(fun x y -> x + y) (1 + 2) (3 * 4)"
      (function
        | Let
            (_, Apply (Primitive Multiply, _, _), Let (_, Apply (Primitive Add, _, _), _))
          -> true
        | _ -> false)
      "Expected multiplication before addition"
  ; shape
      "ANF evaluates tuple fields right to left"
      "(1 + 2, 3 * 4)"
      (function
        | Let
            ( _
            , Apply (Primitive Multiply, _, _)
            , Let (_, Apply (Primitive Add, _, _), Return (Tuple _)) ) -> true
        | _ -> false)
      "Expected multiplication before addition before the tuple construction"
  ; shape
      "ANF does not duplicate the continuation of an if"
      "(if 1 = 1 then 10 else 20) + 2"
      (function
        | Let
            ( _
            , Apply (Primitive Equal, _, _)
            , Let (result, If _, Return (Apply (Primitive Add, Var arg, [ _ ]))) ) ->
          Id.equal result arg
        | _ -> false)
      "Expected a single addition after the conditional"
  ; shape
      "ANF retains the recursive scope around initializer computations"
      "let rec f = let x = 1 in fun y -> if y = 0 then x else f (y - 1) in f 3"
      (function
        | Let_rec (f, Return (Function _), Return (Apply (Var callee, _, []))) ->
          Id.equal f callee
        | _ -> false)
      "Expected recursion to enclose the complete initializer"
  ; shape
      "ANF retains recursive tuple initialization within its scope"
      "let rec p = ((fun x -> x + 1), (fun x -> let (f, _) = p in f x)) in let (_, g) = \
       p in g 41"
      (function
        | Let_rec (_, Let (_, Function _, _), _) -> true
        | _ -> false)
      "Expected closure-producing computations inside the recursive initializer"
  ]
;;

let invalid name construct expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let term, error = construct () in
    match Anf.validate term with
    | Error actual when actual = error -> true
    | Error actual -> fail name ("Wrong invariant failure: " ^ invariant_error actual)
    | Ok () -> fail name expected)
;;

let unit_term = Anf.Return (Atom (Constant Unit))

let invariants =
  [ invalid
      "ANF rejects an unbound result"
      (fun () ->
         let id, _ = fresh Id.initial "missing" in
         Anf.Return (Atom (Var id)), Lambda.Unbound_variable id)
      "Expected an unbound-variable error"
  ; invalid
      "ANF nonrecursive initializer cannot use its own binding"
      (fun () ->
         let id, _ = fresh Id.initial "self" in
         Anf.Let (id, Atom (Var id), unit_term), Lambda.Unbound_variable id)
      "Expected initializer scope to exclude the nonrecursive binder"
  ; invalid
      "ANF branch bindings cannot escape their branch"
      (fun () ->
         let id, _ = fresh Id.initial "branch" in
         ( Anf.Sequence
             ( If
                 ( Constant (Bool true)
                 , Let (id, Atom (Constant Unit), unit_term)
                 , unit_term )
             , Return (Atom (Var id)) )
         , Lambda.Unbound_variable id ))
      "Expected branch-local binder to stay local"
  ; invalid
      "ANF rejects a negative tuple projection"
      (fun () -> Anf.Return (Field (Constant Unit, -1)), Lambda.Negative_field (-1))
      "Expected a negative-field error"
  ; invalid
      "ANF rejects a repeated sequential binder"
      (fun () ->
         let id, _ = fresh Id.initial "repeated" in
         ( Anf.Let (id, Atom (Constant Unit), Let (id, Atom (Constant Unit), unit_term))
         , Lambda.Duplicate_binding id ))
      "Expected globally distinct binding identities"
  ; invalid
      "ANF rejects a binder repeated across disjoint branches"
      (fun () ->
         let id, _ = fresh Id.initial "branch" in
         let branch = Anf.Let (id, Atom (Constant Unit), unit_term) in
         ( Anf.Return (If (Constant (Bool true), branch, branch))
         , Lambda.Duplicate_binding id ))
      "Expected binding identity to be unique even across exclusive branches"
  ; invalid
      "ANF rejects repeated function parameters"
      (fun () ->
         let id, _ = fresh Id.initial "arg" in
         ( Anf.Return (Function { parameter = id; parameters = [ id ]; body = unit_term })
         , Lambda.Duplicate_binding id ))
      "Expected distinct parameter identities"
  ; invalid
      "ANF cannot reuse a function-local binder in the continuation"
      (fun () ->
         let id, _ = fresh Id.initial "shared" in
         ( Anf.Sequence
             ( Function { parameter = id; parameters = []; body = unit_term }
             , Let (id, Atom (Constant Unit), unit_term) )
         , Lambda.Duplicate_binding id ))
      "Expected declared identities to survive the end of a lexical scope"
  ; invalid
      "ANF cannot reuse a recursive-initializer binder in its body"
      (fun () ->
         let rec_id, supply = fresh Id.initial "recursive" in
         let id, _ = fresh supply "shared" in
         ( Anf.Let_rec
             ( rec_id
             , Let (id, Atom (Constant Unit), unit_term)
             , Let (id, Atom (Constant Unit), unit_term) )
         , Lambda.Duplicate_binding id ))
      "Expected initializer declarations to participate in global uniqueness"
  ]
;;

(* Generate source expressions with their expected integer results. *)
let rec arithmetic depth =
  let open QCheck.Gen in
  let ( let* ) = ( >>= ) in
  let literal = map (fun n -> Printf.sprintf "(%d)" n, n) (int_range (-10) 10) in
  if depth = 0
  then literal
  else
    oneof_weighted
      [ 2, literal
      ; ( 3
        , let* a, av = arithmetic (depth - 1) in
          let* b, bv = arithmetic (depth - 1) in
          return (Printf.sprintf "(%s + %s)" a b, av + bv) )
      ; ( 2
        , let* a, av = arithmetic (depth - 1) in
          let* b, bv = arithmetic (depth - 1) in
          return (Printf.sprintf "(let x = %s in x - %s)" a b, av - bv) )
      ; ( 2
        , let* a, av = arithmetic (depth - 1) in
          let* b, bv = arithmetic (depth - 1) in
          let* choose = bool in
          return
            ( Printf.sprintf "(if %b then %s else %s)" choose a b
            , if choose then av else bv ) )
      ; ( 1
        , let* a, av = arithmetic (depth - 1) in
          let* b, bv = arithmetic (depth - 1) in
          return (Printf.sprintf "((fun x y -> x + y) %s %s)" a b, av + bv) )
      ]
;;

let arithmetic_property =
  QCheck.Test.make
    ~name:"generated nested computations retain their specified results in ANF"
    ~count:1000
    (QCheck.make
       ~print:(fun (source, expected) -> source ^ " = " ^ string_of_int expected)
       (arithmetic 4))
    (fun (source, expected) ->
       equal
         source
         (string_of_int expected)
         (observe source Lambda_eval.observe (Anf.to_lambda (normalize source))))
;;

let printer_stability =
  QCheck.Test.make
    ~name:"ANF output is independent of prior Id allocation"
    ~count:1000
    QCheck.int_small
    (fun n ->
       let source = Printf.sprintf "let f x = x + (%d) in f 2" n in
       let first, supply = normalize_with_supply Id.initial source in
       let second, _ = normalize_with_supply supply source in
       let first = Format.asprintf "%a" Anf_print.expression first in
       let second = Format.asprintf "%a" Anf_print.expression second in
       equal source first second)
;;

let checked_traversal_error =
  QCheck.Test.make
    ~name:"checked traversal returns the first callback error in branch order"
    ~count:1
    QCheck.unit
    (fun () ->
       let first, supply = fresh Id.initial "first" in
       let second, _ = fresh supply "second" in
       let function_ parameter : Anf.function_definition =
         { parameter; parameters = []; body = Return (Atom (Var parameter)) }
       in
       let expression =
         Anf.Return
           (If
              ( Constant (Bool true)
              , Return (Function (function_ first))
              , Return (Function (function_ second)) ))
       in
       match
         Anf.map_accum_result
           ~atom:Fun.id
           ~function_:(fun () (definition : Anf.function_definition) ->
             Error definition.parameter)
           ()
           expression
       with
       | Error id when Id.equal id first -> true
       | Error id ->
         fail
           "if true then first else second"
           ("Expected first callback error, got " ^ Id.name id)
       | Ok _ -> fail "if true then first else second" "Expected the callback error")
;;

let () =
  run_tests
    (behaviors
     @ shapes
     @ invariants
     @ [ checked_traversal_error ]
     @ [ arithmetic_property; printer_stability ])
;;
