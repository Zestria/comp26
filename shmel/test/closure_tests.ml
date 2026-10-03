[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_lib
open Shmel_middleend
module A = Anf
module C = Closure_ir
module G = QCheck.Gen

let test name f = QCheck.Test.make ~count:1 ~name QCheck.unit f
let fail fmt = QCheck.Test.fail_reportf fmt

let parse source =
  match Main.parse_program_with_comb source with
  | Ok p -> p
  | Error (`Parsing_error message) -> fail "Parse error: %s\n%s" message source
;;

let same_value a b =
  match a, b with
  | Eval.VInt a, Closure_eval.VInt b -> a = b
  | Eval.VBool a, Closure_eval.VBool b -> Bool.equal a b
  | Eval.VUnit, Closure_eval.VUnit -> true
  | _ -> false
;;

let equivalent (before : Eval.outcome) (after : Closure_eval.outcome) =
  String.equal before.output after.output
  &&
  match before.result, after.result with
  | Ok a, Ok b -> same_value a b
  | Error a, Error b -> a = b
  | _ -> false
;;

let show_before (o : Eval.outcome) =
  let result =
    match o.result with
    | Ok v -> Format.asprintf "%a" Eval.pp_value v
    | Error e -> Format.asprintf "error: %a" Eval.pp_error e
  in
  Printf.sprintf "output=%S result=%s" o.output result
;;

let show_after (o : Closure_eval.outcome) =
  let result =
    match o.result with
    | Ok v -> Format.asprintf "%a" Closure_eval.pp_value v
    | Error e -> Format.asprintf "error: %a" Eval.pp_error e
  in
  Printf.sprintf "output=%S result=%s" o.output result
;;

let check anf =
  let converted = Closure_conversion.of_program anf in
  let before = Eval.run (A.to_program anf) in
  let after = Closure_eval.run converted in
  if not (equivalent before after)
  then
    fail
      "Before: %s\nAfter: %s\n%a"
      (show_before before)
      (show_after after)
      C.pp_program
      converted;
  after
;;

let examples =
  [ "no captures", "let f x = x + 1 in f 4", "", 5
  ; ( "independent closures"
    , "let make x = fun y -> x + y in let f = make 10 in let g = make 20 in f 1 + g 1"
    , ""
    , 32 )
  ; ( "nested captures"
    , "let make x = fun y -> fun z -> x + y + z in let f = make 10 in let g = f 20 in g 3"
    , ""
    , 33 )
  ; ( "captured function"
    , "let f x = x + 2 in let make g = fun x -> g x + 1 in let h = make f in h 4"
    , ""
    , 7 )
  ; ( "recursive capture"
    , "let make x = let rec sum n = if n <= 0 then x else x + sum (n - 1) in sum in let \
       f = make 3 in f 4"
    , ""
    , 15 )
  ; ( "recursive function passed as value"
    , "let run f n = f n in let rec f n = if n <= 0 then 7 else run f (n - 1) in f 4"
    , ""
    , 7 )
  ; ( "recursive partial application"
    , "let rec sum x n = if n <= 0 then x else sum x (n - 1) + 1 in let f = sum 10 in f 4"
    , ""
    , 14 )
  ; ( "partial application delays effects"
    , "let f x y = let _ = print_int x in y in let g = f 3 in let _ = print_int 1 in g 7"
    , "13"
    , 7 )
  ; "overapplication", "let make x = let y = x + 1 in fun z -> y + z in make 10 2", "", 13
  ; ( "builtin shadowing"
    , "let p = print_int in let print_int x = x + 1 in let _ = p 4 in print_int 5"
    , "4"
    , 6 )
  ; ( "fresh names"
    , "let cc_self0 = 5 in let cc_code0 x = x + cc_self0 in cc_code0 3"
    , ""
    , 8 )
  ; ( "top-level shadowing"
    , "let x = 10\nlet f y = x + y\nlet x = 100\nlet main = f 1"
    , ""
    , 11 )
  ; ( "CPS factorial"
    , "let rec fac n k = if n <= 1 then k 1 else fac (n - 1) (fun x -> k (n * x)) in fac \
       6 (fun x -> x)"
    , ""
    , 720 )
  ; ( "CPS fibonacci"
    , "let rec fib n k = if n <= 1 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun \
       b -> k (a + b))) in fib 6 (fun x -> x)"
    , ""
    , 8 )
  ]
;;

let example (name, source, output, result) =
  test name (fun () ->
    let original = parse source in
    let actual = check (A.of_program original) in
    let before = Eval.run original in
    if not (equivalent before actual)
    then fail "Source: %s\nClosure: %s" (show_before before) (show_after actual);
    String.equal actual.output output && actual.result = Ok (Closure_eval.VInt result))
;;

let v x = A.ImmVar x
let int n = A.ImmInt n
let ret x = A.AExpr (A.CImm x)

let structural =
  [ test "let binds only its body" (fun () ->
      Closure_conversion.free_variables (A.ALet ("x", A.CImm (v "x"), ret (v "x")))
      = [ "x" ])
  ; test "environment contains each needed value once" (fun () ->
      let body =
        A.ALet
          ( "t"
          , A.CBinOp (Ast.Add, v "z", v "a")
          , A.AExpr (A.CBinOp (Ast.Add, v "z", v "t")) )
      in
      let p =
        [ A.DLet ("z", [], ret (int 10))
        ; A.DLet ("a", [], ret (int 2))
        ; A.DLet ("unused", [], ret (int 99))
        ; A.DLet ("f", [ "x" ], body)
        ]
      in
      match List.rev (Closure_conversion.of_program p) with
      | C.DLet (_, C.AExpr (C.MakeClosure (code, fields))) :: _ ->
        code.captures = [ "a"; "z" ] && fields = [ v "a"; v "z" ]
      | _ -> false)
  ; test "error preserves preceding output" (fun () ->
      let p = parse "let f x = let _ = print_int x in x / 0 in f 4" in
      let actual = check (A.of_program p) in
      actual.output = "4" && actual.result = Error Eval.Division_by_zero)
  ]
;;

type ty =
  | TInt
  | TFun of ty * ty

let names = [ "x"; "y"; "f"; "g"; "cc_self0"; "cc_code0"; "t0" ]

let types =
  [ TInt
  ; TFun (TInt, TInt)
  ; TFun (TInt, TFun (TInt, TInt))
  ; TFun (TFun (TInt, TInt), TInt)
  ]
;;

let extend env x ty = (x, ty) :: List.remove_assoc x env

let rec gen ty env size =
  let open G in
  let variables =
    List.filter_map (fun (x, t) -> if t = ty then Some (Ast.Var x) else None) env
  in
  let base =
    match ty with
    | TInt -> map (fun n -> Ast.Const n) (int_range (-10) 10)
    | TFun (a, b) ->
      let* x = oneof_list names in
      let+ body = gen b (extend env x a) (max 0 (size - 1)) in
      Ast.Abs (x, body)
  in
  if size <= 0
  then if variables = [] then base else oneof [ base; oneof_list variables ]
  else
    let* kind = oneof_list [ 0; 1; 2; 3; 4 ] in
    match kind with
    | 0 -> if variables = [] then base else oneof [ base; oneof_list variables ]
    | 1 ->
      let* x = oneof_list names in
      let* t = oneof_list types in
      let* rhs = gen t env (size / 2) in
      let+ body = gen ty (extend env x t) (size / 2) in
      Ast.Let (x, rhs, body)
    | 2 ->
      let* t = oneof_list types in
      let* f = gen (TFun (t, ty)) env (size / 2) in
      let+ arg = gen t env (size / 2) in
      Ast.App (f, arg)
    | 3 ->
      let* a = gen TInt env (size / 3) in
      let* b = gen TInt env (size / 3) in
      let* t = gen ty env (size / 3) in
      let+ e = gen ty env (size / 3) in
      Ast.If (Ast.BinOp (Ast.Lt, a, b), t, e)
    | _ ->
      (match ty with
       | TFun _ -> base
       | TInt ->
         let* a = gen TInt env (size / 2) in
         let+ b = gen TInt env (size / 2) in
         Ast.BinOp (Ast.Add, a, b))
;;

let generated =
  let open G in
  let* size = int_range 0 9 in
  let+ e = gen TInt [] size in
  [ Ast.DExpr e ]
;;

let random_program =
  let open G in
  let* size = int_range 0 9 in
  let+ e = gen TInt [] size in
  [ Ast.DExpr e ]
;;

let random_test =
  QCheck.Test.make
    ~count:1000
    ~name:"random functions preserve result and output"
    (QCheck.make ~print:Printast.show_program random_program)
    (fun p ->
       let _ = check (A.of_program p) in
       true)
;;

let () =
  QCheck_base_runner.run_tests_main
    (List.map example examples @ structural @ [ random_test ])
;;
