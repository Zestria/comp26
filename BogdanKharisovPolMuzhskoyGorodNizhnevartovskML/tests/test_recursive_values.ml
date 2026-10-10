[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend
open Middleend
open Middleend_test_support

let accepts name source expected =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let ast = parsed Parser.parse_expression source in
    (match Recursive_check.expression ast with
     | Ok () -> ()
     | Error e -> fail source (Recursive_check.error_to_string e));
    let term, supply = lower_with_supply Id.initial source in
    let _ = equal source expected (observe source Lambda_eval.observe term) in
    let normalized, _ = allocated source (Anf.of_lambda supply term) in
    equal source expected (observe source Lambda_eval.observe (Anf.to_lambda normalized)))
;;

let rejects name source =
  QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
    let ast = parsed Parser.parse_expression source in
    match Recursive_check.expression ast with
    | Error _ -> true
    | Ok () -> fail source "Expected rejection of unsafe recursive initialization")
;;

let cases =
  [ accepts
      "recursive pair with a captured back edge"
      "let rec pair = ((fun x -> x + 1), (fun x -> let (f, _) = pair in f x)) in let (_, \
       g) = pair in g 41"
      "42"
  ; accepts
      "recursive tuple and scalar field"
      "let rec p = (42, fun () -> let (x, _) = p in x) in let (_, f) = p in f ()"
      "42"
  ; accepts
      "recursive function returned through a local alias"
      "let rec f = let g = fun n -> if n = 0 then 42 else f (n - 1) in g in f 3"
      "42"
  ; accepts "independent dynamic recursive initializer" "let rec x = 40 + 2 in x" "42"
  ; accepts
      "unused recursive initializer still executes eagerly"
      "let rec x = 1 / 0 in 42"
      "Division_by_zero"
  ; accepts
      "discarding a recursive address does not inspect it"
      "let rec x = let _ = x in 42 in x"
      "42"
  ; accepts
      "unused recursive address alias is guarded"
      "let rec x = let x = x in 42 in x"
      "42"
  ; accepts
      "a local binding can shadow a recursive name"
      "let rec x = let x = 42 in (x, fun () -> x) in let (_, f) = x in f ()"
      "42"
  ; rejects "a recursive value cannot be its own initializer" "let rec x = x in x"
  ; rejects
      "a recursive value cannot be inspected before initialization"
      "let rec x = x + 1 in x"
  ; rejects "an alias cannot hide unguarded recursion" "let rec x = let y = x in y in x"
  ; rejects
      "a unit pattern inspects its input before initialization"
      "let rec x = let () = x in () in x"
  ; rejects
      "an unused tuple pattern still inspects its input"
      "let rec p = let (_, _) = p in (1, 2) in p"
  ; rejects
      "a dynamically sized function cannot capture its recursive name"
      "let rec f = if true then (fun x -> f x) else (fun x -> x) in f"
  ; rejects
      "a local alias does not make dynamic size static"
      "let rec f = let g = if true then (fun x -> f x) else (fun x -> x) in g in f"
  ; rejects
      "calling a nested recursive function can inspect an outer recursive value"
      "let rec p = let rec g = fun () -> let (x, _) = p in x in (g (), g) in p"
  ]
;;

let nested_reader depth =
  let base =
    "let rec g0 n = if n = 0 then let (value, _) = p in value else g0 (n - 1) in "
  in
  base
  ^ String.concat
      ""
      (List.init depth (fun i ->
         Printf.sprintf
           "let rec g%d n = if n = 0 then g%d 0 else g%d (n - 1) in "
           (i + 1)
           i
           (i + 1)))
  ^ Printf.sprintf "g%d" depth
;;

let recursive_inputs =
  QCheck.(triple (int_range 1 5) (int_range 0 6) (int_range (-100) 100))
;;

let generated_accepts name make_source =
  QCheck.Test.make ~name ~count:500 recursive_inputs (fun input ->
    let source, expected = make_source input in
    let ast = parsed Parser.parse_expression source in
    match Pipeline.compile_expression ast with
    | Error error -> fail source (Pipeline.error_to_string error)
    | Ok program ->
      equal source (string_of_int expected) (observe source Anf_eval.lifted program))
;;

let generated_rejects name make_source =
  QCheck.Test.make ~name ~count:500 recursive_inputs (fun input ->
    let source = make_source input in
    match Recursive_check.expression (parsed Parser.parse_expression source) with
    | Error (Unsafe_recursive_definition name) when Name.to_string name = "p" -> true
    | Error error ->
      fail
        source
        ("Wrong recursive definition rejected: " ^ Recursive_check.error_to_string error)
    | Ok () -> fail source "Expected rejection of an unsafe recursive initializer")
;;

let recursive_families =
  [ generated_accepts
      "generated nested recursive captures may read the initialized outer tuple"
      (fun (depth, calls, value) ->
         ( Printf.sprintf
             "let rec p = ((%d), (%s)) in let (_, read) = p in read %d"
             value
             (nested_reader depth)
             calls
         , value ))
  ; generated_rejects
      "generated nested recursive captures cannot read the outer tuple during \
       initialization"
      (fun (depth, calls, _) ->
         Printf.sprintf
           "let rec p = let read = (%s) in (read %d, read) in p"
           (nested_reader depth)
           calls)
  ; generated_accepts
      "generated recursive captures resolve a shadowing scalar instead of the outer tuple"
      (fun (depth, calls, value) ->
         let functions =
           "let rec g0 n = if n = 0 then p else g0 (n - 1) in "
           ^ String.concat
               ""
               (List.init depth (fun i ->
                  Printf.sprintf
                    "let rec g%d n = if n = 0 then g%d 0 else g%d (n - 1) in "
                    (i + 1)
                    i
                    (i + 1)))
           ^ Printf.sprintf "g%d" depth
         in
         ( Printf.sprintf
             "let rec p = let p = (%d) in let read = (%s) in (read %d, read) in let \
              (value, _) = p in value"
             value
             functions
             calls
         , value ))
  ; generated_rejects
      "generated dynamically selected recursive functions cannot retain a self reference"
      (fun (depth, calls, value) ->
         let function_ =
           Printf.sprintf "fun n -> if n = 0 then (%d) else p (n - 1)" value
         in
         let rhs =
           List.fold_left
             (fun rhs i -> Printf.sprintf "let f%d = (%s) in f%d" i rhs i)
             (Printf.sprintf
                "if %b then (%s) else (%s)"
                (calls mod 2 = 0)
                function_
                function_)
             (List.init depth Fun.id)
         in
         Printf.sprintf "let rec p = %s in p %d" rhs calls)
  ]
;;

let () = run_tests (cases @ recursive_families)
