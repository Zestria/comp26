[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Middleend
open Frontend.Constant
open Middleend_test_support

let case name primitive arguments expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let show = function
      | Bool value -> string_of_bool value
      | Unit -> "()"
      | Int value -> Frontend.Integer_literal.to_string value
    in
    let result = Primitive_simplify.apply primitive arguments in
    let render = function
      | None -> "keep call"
      | Some value -> show value
    in
    equal
      (Primitive.name primitive ^ " " ^ String.concat " " (List.map show arguments))
      (render expected)
      (render result))
;;

let truth_table primitive (ff, ft, tf, tt, unit) =
  let test left right expected =
    case
      (Printf.sprintf "fold %b %s %b" left (Primitive.name primitive) right)
      primitive
      [ Bool left; Bool right ]
      (Some (Bool expected))
  in
  [ test false false ff
  ; test false true ft
  ; test true false tf
  ; test true true tt
  ; case ("fold unit " ^ Primitive.name primitive) primitive [ Unit; Unit ] unit
  ]
;;

let shape name source predicate expectation =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let lambda, supply = lower_with_supply Id.initial source in
    let result, _ = allocated source (Anf.of_lambda supply lambda) in
    if not (predicate result)
    then
      fail
        source
        (expectation ^ "\nActual: " ^ Format.asprintf "%a" Anf_print.expression result);
    true)
;;

let behavior name source expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let lambda, supply = lower_with_supply Id.initial source in
    let normalized, _ = allocated source (Anf.of_lambda supply lambda) in
    equal source expected (observe source Lambda_eval.observe (Anf.to_lambda normalized)))
;;

let () =
  run_tests
    (List.concat
       [ truth_table Primitive.Equal (true, false, false, true, Some (Bool true))
       ; truth_table Not_equal (false, true, true, false, Some (Bool false))
       ; truth_table Less (false, true, false, false, Some (Bool false))
       ; truth_table Less_equal (true, true, false, true, Some (Bool true))
       ; truth_table Greater (false, false, true, false, Some (Bool false))
       ; truth_table Greater_equal (true, false, true, true, Some (Bool true))
       ; truth_table And (false, false, false, true, None)
       ; truth_table Or (false, true, true, true, None)
       ; [ case "fold not true" Not [ Bool true ] (Some (Bool false))
         ; case "fold not false" Not [ Bool false ] (Some (Bool true))
         ; case "retain unsaturated boolean primitive" And [ Bool true ] None
         ; case "retain overapplied boolean primitive" Not [ Bool true; Bool false ] None
         ; case "retain primitive with no operands" Not [] None
         ; case "retain incompatible comparison types" Equal [ Bool true; Unit ] None
         ; case "retain non-boolean not operand" Not [ Unit ] None
         ; case
             "retain arithmetic with boolean operands"
             Add
             [ Bool true; Bool false ]
             None
         ; shape
             "known primitive facts select a branch through aliases"
             "let inverse = not in let b = inverse false in if b then 42 else 1 / 0"
             (function
               | Anf.Return (Atom (Constant (Int value))) ->
                 Frontend.Integer_literal.to_string value = "42"
               | _ -> false)
             "Expected the selected constant without the unreachable call"
         ; shape
             "discard an unused proven-total comparison"
             "let unused = true <= false in 42"
             (function
               | Anf.Return (Atom (Constant (Int value))) ->
                 Frontend.Integer_literal.to_string value = "42"
               | _ -> false)
             "Expected no computation of the unused comparison"
         ; shape
             "preserve redefined comparison as an ordinary call"
             "let (<=) x y = x || y in true <= false"
             (function
               | Anf.Let (_, Function _, Return (Apply (Var _, _, _))) -> true
               | _ -> false)
             "Expected a call to the lexical binding"
         ; shape
             "retain potentially raising comparison of function values"
             "let f x = x in let unused = f = f in 42"
             (function
               | Anf.Let (_, Function _, Sequence (Apply (Primitive Equal, _, _), _)) ->
                 true
               | _ -> false)
             "Expected the unused comparison to remain eager"
         ; behavior
             "folding a strict logical alias retains effects before its known argument"
             "let both = (&&) in both false (let _ = 1 / 0 in true)"
             "Division_by_zero"
         ; behavior
             "discarding a folded result retains the argument's eager effects"
             "let unused = not (let _ = 1 / 0 in true) in 42"
             "Division_by_zero"
         ]
       ])
;;
