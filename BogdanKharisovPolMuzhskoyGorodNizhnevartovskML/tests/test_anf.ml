[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_middleend
open Bkml_middleend_test_support

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
  ; behavior
      "ANF retains evaluation of a discarded tuple field"
      "let (answer, _) = (42, 1 / 0) in answer"
      "Division_by_zero"
  ; behavior
      "ANF retains effects before a known condition"
      "if (let _ = 1 / 0 in true) then 42 else 0"
      "Division_by_zero"
  ; behavior
      "ANF tuple facts distinguish shadowed bindings"
      "let p = (40, 0) in let f () = let (a, _) = p in a in let p = (2, 0) in let (b, _) \
       = p in f () + b"
      "42"
  ; behavior
      "ANF keeps both live closures in an unknown conditional"
      "let f () = 40 in let g () = 2 in let choose b = if b then f () else g () in \
       choose true + choose false"
      "42"
  ; behavior
      "ANF does not leak tuple facts from one unknown branch into another"
      "let choose b = let p = if b then (10, 0) else (32, 0) in let (x, _) = p in x in \
       choose true + choose false"
      "42"
  ; behavior "ANF CPS factorial" (cps_factorial 7) "5040"
  ; behavior "ANF CPS factorial base case" (cps_factorial 0) "1"
  ; behavior "ANF CPS factorial first recursive step" (cps_factorial 1) "1"
  ; behavior "ANF CPS fibonacci" (cps_fibonacci 10) "55"
  ; behavior "ANF CPS fibonacci zero base case" (cps_fibonacci 0) "0"
  ; behavior "ANF CPS fibonacci one base case" (cps_fibonacci 1) "1"
  ]
;;

let returns_int expected = function
  | Anf.Return (Atom (Constant (Int n))) ->
    Bkml_frontend.Integer_literal.to_string n = string_of_int expected
  | _ -> false
;;

let shapes =
  [ shape
      "ANF eliminates chains of atomic aliases"
      "let x = 42 in let y = x in y"
      (function
        | Return (Atom (Constant (Int n))) ->
          Bkml_frontend.Integer_literal.to_string n = "42"
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
  ; shape
      "ANF selects a constant true branch"
      "if true then 42 else 1 / 0"
      (returns_int 42)
      "Expected 42 without a conditional or its unreachable branch"
  ; shape
      "ANF selects a constant false branch through an alias"
      "let b = false in if b then 1 / 0 else 42"
      (returns_int 42)
      "Expected 42 without a conditional or its unreachable branch"
  ; shape
      "ANF propagates fields of nested tuples"
      "let p = (10, (20, 42)) in let (_, (_, answer)) = p in answer"
      (returns_int 42)
      "Expected the selected field without tuple allocations or projections"
  ; shape
      "ANF propagates a middle field of a tuple with more than two fields"
      "let (_, answer, _) = (0, 42, 1) in answer"
      (returns_int 42)
      "Expected 42 without tuple allocations or projections"
  ; shape
      "ANF propagates the last field beyond a tuple's first two fields"
      "let (_, _, answer) = (0, 1, 42) in answer"
      (returns_int 42)
      "Expected 42 without tuple allocations or projections"
  ; shape
      "ANF propagates a tuple returned from a selected branch"
      "let p = if true then let x = 40 + 1 in (x, 0) else (0, 0) in let (a, _) = p in a \
       + 1"
      (function
        | Let
            (x, Apply (Primitive Add, _, _), Return (Apply (Primitive Add, Var a, [ _ ])))
          -> Id.equal x a
        | _ -> false)
      "Expected two additions sharing one intermediate, with no tuple or branch"
  ; shape
      "ANF preserves a tuple that also escapes as a value"
      "let p = (1, 2) in let (a, _) = p in (p, a)"
      (function
        | Let (p, Tuple _, Return (Tuple (Var result, Constant (Int a), []))) ->
          Id.equal p result && Bkml_frontend.Integer_literal.to_string a = "1"
        | _ -> false)
      "Expected one retained tuple binding and a simplified projection"
  ; shape
      "ANF propagates a known field into a closure"
      "let p = (42, 0) in fun () -> let (a, _) = p in a"
      (function
        | Return (Function { body; _ }) -> returns_int 42 body
        | _ -> false)
      "Expected a function returning 42 without capturing the tuple"
  ; shape
      "ANF removes transitive dead closure dependencies"
      "let f () = 1 / 0 in let unused () = f () in 42"
      (returns_int 42)
      "Expected no functions, including the one used only by a dead closure"
  ; shape
      "ANF removes a discarded tuple and its unused function field"
      "let _ = (fun () -> 1 / 0), 0 in 42"
      (returns_int 42)
      "Expected no allocation and no evaluation of the discarded function body"
  ; shape
      "ANF retains computations before a discarded tuple allocation"
      "let unused = (1 + 2, 3 * 4) in 42"
      (function
        | Sequence
            ( Apply (Primitive Multiply, _, _)
            , Sequence (Apply (Primitive Add, _, _), result) ) -> returns_int 42 result
        | _ -> false)
      "Expected multiplication and addition in order, without their dead bindings"
  ; shape
      "ANF preserves an unused call that can diverge"
      "let rec loop x = loop x in let unused = loop 0 in 42"
      (function
        | Let_rec (loop, _, Sequence (Apply (Var callee, _, []), result)) ->
          Id.equal loop callee && returns_int 42 result
        | _ -> false)
      "Expected the eager call to remain before 42"
  ; shape
      "ANF retains an unknown tuple projection with an unused result"
      "fun p -> let (a, b) = p in 42"
      (function
        | Return (Function { body = Sequence (Field _, Sequence (Field _, result)); _ })
          -> returns_int 42 result
        | _ -> false)
      "Expected unknown projections to remain until their safety is established"
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
      ; ( 2
        , let* a, av = arithmetic (depth - 1) in
          let* b, bv = arithmetic (depth - 1) in
          return
            ( Printf.sprintf
                "(let p = (%s, %s) in let f () = let (x, y) = p in x + y in f ())"
                a
                b
            , av + bv ) )
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

(* Choose an arbitrary field of an immutable tuple. The expected value comes
   from the generated source data; no normalization algorithm is duplicated.
   Nesting the selected binding exercises tuple facts across hoisted scopes. *)
let tuple_projection_property =
  let inputs =
    let open QCheck.Gen in
    let ( let* ) = ( >>= ) in
    let* length = int_range 2 20 in
    let* values = list_size (return length) (int_range 0 1000) in
    let* selected = int_range 0 (length - 1) in
    let* choice = bool in
    let tuple = "(" ^ String.concat ", " (List.map string_of_int values) ^ ")" in
    let pattern =
      "("
      ^ String.concat
          ", "
          (List.mapi (fun i _ -> if i = selected then "x" else "_") values)
      ^ ")"
    in
    let selected_branch = "let inner = " ^ tuple ^ " in inner" in
    let unused_branch = "let _ = 1 / 0 in " ^ tuple in
    let yes, no =
      if choice then selected_branch, unused_branch else unused_branch, selected_branch
    in
    return
      ( Printf.sprintf
          "let p = if %b then (%s) else (%s) in let alias = p in let %s = alias in x"
          choice
          yes
          no
          pattern
      , List.nth values selected )
  in
  QCheck.Test.make
    ~name:"generated known tuple projections collapse to the selected constant"
    ~count:1000
    (QCheck.make
       ~print:(fun (source, expected) -> source ^ " = " ^ string_of_int expected)
       inputs)
    (fun (source, expected) ->
       let result = normalize source in
       if not (returns_int expected result)
       then
         fail
           source
           (Printf.sprintf
              "Expected only %d, without administrative code.\nActual ANF:\n%s"
              expected
              (Format.asprintf "%a" Anf_print.expression result));
       true)
;;

(* Field indices beyond a known tuple's size must not be folded or silently
   dropped. Such IR cannot arise from a well-typed tuple pattern, but the ANF
   validator checks nonnegativity only, so normalization must handle it. *)
let out_of_range_projection name index =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let input = Lambda.Field (Lambda.Tuple (Constant Unit, Constant Unit, []), index) in
    let source = Printf.sprintf "field[%d] ((), ())" index in
    let result, _ = allocated source (Anf.of_lambda Id.initial (checked source input)) in
    match result with
    | Let (tuple, Tuple _, Return (Field (Var value, actual)))
      when Id.equal tuple value && actual = index -> true
    | _ -> fail source "Expected the out-of-range projection to remain in ANF")
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
           ~callee:(fun atom ~arity:_ -> atom)
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
     @ [ arithmetic_property
       ; printer_stability
       ; tuple_projection_property
       ; out_of_range_projection "ANF retains a field just beyond tuple size" 2
       ; out_of_range_projection "ANF retains a maximal field index" max_int
       ])
;;
