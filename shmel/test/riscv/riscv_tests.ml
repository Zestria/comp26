[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Code generation for function bodies, checked under QEMU: every case is a
   function [let f p1 ... pn = body] without calls, its result is compared
   with the reference interpreter. All the cases go into one assembly file,
   so QEMU starts once.

   [riscv_tests.exe asm] prints the code of the functions of the program on
   stdin instead. *)

open Shmel_lib
open Shmel_middleend
open Shmel_backend
module Gen = QCheck.Gen

type case =
  { name : string
  ; program : Ast.program (** defines [f] *)
  ; args : int list
  }

let call_f args =
  Ast.DExpr (List.fold_left (fun f a -> Ast.App (f, Ast.Const a)) (Ast.Var "f") args)
;;

(* [None] when the program fails (division by zero) or the number is so big
   that 63-bit OCaml integers and 64-bit registers may disagree. *)
let expected case =
  match (Eval.run (case.program @ [ call_f case.args ])).result with
  | Ok (Eval.VInt n) when abs n < 1 lsl 40 -> Some n
  | Ok (Eval.VBool b) -> Some (Bool.to_int b)
  | Ok _ | Error _ -> None
;;

let function_of program =
  List.find_map
    (function
      | Anf.DLet ("f", params, body) | Anf.DLetRec ("f", params, body) ->
        Some (params, body)
      | Anf.DLet _ | Anf.DLetRec _ | Anf.DExpr _ -> None)
    (Anf.of_program program)
;;

let compile ~name program =
  match function_of program with
  | None -> Error "no function f"
  | Some (params, body) -> Codegen.compile_function Codegen.no_calls ~name ~params body
;;

(* The caller side of cdecl: the arguments go on the stack. *)
let entry i args =
  let open Riscv in
  let space = ((8 * List.length args) + 15) / 16 * 16 in
  let adjust n = if n = 0 then [] else [ I (Addi, Sp, Sp, n) ] in
  [ Label (Printf.sprintf "entry%d" i); I (Addi, Sp, Sp, -16); Sd (Ra, 8, Sp) ]
  @ adjust (-space)
  @ List.concat (List.mapi (fun k a -> [ Li (T 0, a); Sd (T 0, 8 * k, Sp) ]) args)
  @ [ Call (Printf.sprintf "f%d" i) ]
  @ adjust space
  @ [ Ld (Ra, 8, Sp); I (Addi, Sp, Sp, 16); Ret ]
;;

let table n =
  let entries = List.init n (fun i -> Printf.sprintf "entry%d" i) in
  Riscv.
    [ Directive ".data"
    ; Directive ".globl shmel_tests_count"
    ; Directive (Printf.sprintf "shmel_tests_count: .dword %d" n)
    ; Directive ".globl shmel_tests"
    ; Directive "shmel_tests:"
    ]
  @ List.map (fun e -> Riscv.Directive ("  .dword " ^ e)) entries
;;

let write_file file text =
  Out_channel.with_open_text file (fun oc -> output_string oc text)
;;

let toolchain_available () =
  List.for_all
    (fun tool -> Sys.command (Printf.sprintf "command -v %s > /dev/null" tool) = 0)
    [ "riscv64-linux-gnu-gcc"; "qemu-riscv64" ]
;;

(* Compiles the cases into one program, runs it, returns the printed lines. *)
let run_on_qemu functions =
  let code =
    (Riscv.Directive ".text"
     :: List.concat (List.mapi (fun i (asm, args) -> asm @ entry i args) functions))
    @ table (List.length functions)
  in
  write_file "tests.s" (Format.asprintf "%a@." Riscv.pp_instrs code);
  let commands =
    [ "riscv64-linux-gnu-gcc -march=rv64gc harness.c tests.s -o tests.exe"
    ; "qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu max ./tests.exe > tests.out"
    ]
  in
  match List.find_opt (fun c -> Sys.command c <> 0) commands with
  | None -> Ok (In_channel.with_open_text "tests.out" In_channel.input_lines)
  | Some failed -> Error (Printf.sprintf "command failed: %s" failed)
;;

(* ---------- hand-written cases ---------- *)

let parse source =
  match Main.parse_program_with_comb source with
  | Ok program -> program
  | Error (`Parsing_error msg) -> [ Ast.DExpr (Ast.Var ("parse error: " ^ msg)) ]
;;

(* Every variable lives until the sum at the end, so the frame is bigger than
   2 KiB and the far slots need a computed address. *)
let many_live_variables n =
  let names = List.init n (fun i -> Printf.sprintf "x%d" i) in
  let lets = List.mapi (fun i x -> Printf.sprintf "let %s = a + %d in" x i) names in
  Printf.sprintf "let f a = %s %s" (String.concat " " lets) (String.concat " + " names)
;;

let unit_cases =
  List.map
    (fun (name, source, args) -> { name; program = parse source; args })
    [ "constant", "let f = 42", []
    ; "negative constant", "let f = -7", []
    ; "big constant", "let f = 1099511627775", []
    ; "parameter", "let f x = x", [ 5 ]
    ; "five parameters", "let f a b c d e = a - b + c * d - e", [ 1; 2; 3; 4; 5 ]
    ; "add", "let f a b = a + b", [ 20; 22 ]
    ; "add small constant", "let f a = a + 100", [ -1 ]
    ; "add big constant", "let f a = a + 100000", [ 1 ]
    ; "constant plus", "let f a = 3 + a", [ 4 ]
    ; "sub", "let f a b = a - b", [ 3; 10 ]
    ; "sub constant", "let f a = a - 2048", [ 0 ]
    ; "sub from constant", "let f a = 1 - a", [ 5 ]
    ; "mul", "let f a b = a * b", [ -6; 7 ]
    ; "div", "let f a b = a / b", [ 17; 5 ]
    ; "div negative", "let f a b = a / b", [ -7; 2 ]
    ; "unary minus", "let f a = - a", [ 9 ]
    ; "double minus", "let f a = - (- a)", [ 9 ]
    ; "minus of expression", "let f a b = - (a * b)", [ 3; 4 ]
    ; "nested", "let f a b c = (a + b) * (b - c) / (a + 1)", [ 5; 7; 2 ]
    ; "less true", "let f a b = a < b", [ 1; 2 ]
    ; "less false", "let f a b = a < b", [ 2; 2 ]
    ; "less constant", "let f a = a < 10", [ 9 ]
    ; "greater", "let f a b = a > b", [ 3; 2 ]
    ; "less or equal", "let f a b = a <= b", [ 2; 2 ]
    ; "greater or equal", "let f a b = a >= b", [ 1; 2 ]
    ; "equal", "let f a b = a = b", [ 4; 4 ]
    ; "equal zero", "let f a = a = 0", [ 0 ]
    ; "if less", "let f n = if n < 2 then 1 else n * 10", [ 5 ]
    ; "if less taken", "let f n = if n < 2 then 1 else n * 10", [ -5 ]
    ; "if equal", "let f a b = if a = b then 100 else 200", [ 3; 3 ]
    ; "if greater", "let f a b = if a > b then a else b", [ 3; 8 ]
    ; "if less or equal", "let f a b = if a <= b then a else b", [ 8; 3 ]
    ; "if greater or equal", "let f a b = if a >= b then 1 else 0", [ 3; 3 ]
    ; "if zero", "let f a = if a = 0 then 7 else 8", [ 0 ]
    ; "if boolean variable", "let f a = let c = a > 3 in if c then c else a < 0", [ 5 ]
    ; ( "boolean used twice"
      , "let f a = let c = a < 3 in let x = (if c then 1 else 2) in if c then x else 0"
      , [ 1 ] )
    ; "nested if", "let f a b = if a < b then (if a < 0 then 1 else 2) else 3", [ 1; 5 ]
    ; "if in let", "let f a = let x = if a < 0 then - a else a in x * 2", [ -21 ]
    ; "if in expression", "let f a b = 1 + (if a < b then a else b) + 2", [ 4; 9 ]
    ; "if in condition", "let f a = if (if a < 0 then 0 else a) > 5 then 1 else 0", [ 9 ]
    ; "let chain", "let f a = let x = a + 1 in let y = x * x in let z = y - a in z", [ 3 ]
    ; "shadowing", "let f a = let a = a + 1 in let a = a * 2 in a", [ 4 ]
    ; "wildcard", "let f a = let _ = a + 1 in a", [ 6 ]
    ; "many live variables", many_live_variables 300, [ 1 ]
    ]
;;

(* ---------- random cases ---------- *)

type ty =
  | Int
  | Bool

let names = [ "x"; "y"; "z"; "a"; "b"; "c" ]
let extend x ty env = (x, ty) :: List.remove_assoc x env

let vars ty env =
  List.filter_map
    (fun (x, t) ->
       match t, ty with
       | Int, Int | Bool, Bool -> Some x
       | Int, Bool | Bool, Int -> None)
    env
;;

let leaf env =
  let open Gen in
  let const = map (fun n -> Ast.Const n) (int_range (-20) 20) in
  match vars Int env with
  | [] -> const
  | xs -> oneof [ const; map (fun x -> Ast.Var x) (oneof_list xs) ]
;;

let rec gen_int env size =
  let open Gen in
  let smaller env = gen_int env (size / 2) in
  if size <= 0
  then leaf env
  else
    let* kind =
      oneof_weighted
        [ 2, return `Leaf
        ; 4, return `Arith
        ; 1, return `Div
        ; 1, return `Neg
        ; 3, return `If
        ; 2, return `Let
        ; 1, return `Let_cond
        ]
    in
    match kind with
    | `Leaf -> leaf env
    | `Arith ->
      map3
        (fun op a b -> Ast.BinOp (op, a, b))
        (oneof_list Ast.[ Add; Sub; Mul ])
        (smaller env)
        (smaller env)
    | `Div -> map2 (fun a b -> Ast.BinOp (Ast.Div, a, b)) (smaller env) (smaller env)
    | `Neg -> map (fun a -> Ast.Neg a) (smaller env)
    | `If ->
      map3
        (fun c t e -> Ast.If (c, t, e))
        (gen_bool env (size / 2))
        (smaller env)
        (smaller env)
    | `Let ->
      let* x = oneof_list names in
      let* e1 = smaller env in
      let+ e2 = smaller (extend x Int env) in
      Ast.Let (x, e1, e2)
    | `Let_cond ->
      let* c = oneof_list names in
      let* cond = gen_bool env (size / 2) in
      let env = extend c Bool env in
      let+ t = smaller env
      and+ e = smaller env in
      Ast.Let (c, cond, Ast.If (Ast.Var c, t, e))

and gen_bool env size =
  let open Gen in
  let comparison =
    map3
      (fun op a b -> Ast.BinOp (op, a, b))
      (oneof_list Ast.[ Eq; Lt; Gt; Le; Ge ])
      (gen_int env size)
      (gen_int env size)
  in
  match vars Bool env with
  | [] -> comparison
  | cs -> oneof_weighted [ 3, comparison; 1, map (fun c -> Ast.Var c) (oneof_list cs) ]
;;

let gen_case i =
  let open Gen in
  let* arity = int_range 0 3 in
  let params = List.init arity (Printf.sprintf "p%d") in
  let* size = int_range 1 12 in
  let* body = gen_int (List.map (fun p -> p, Int) params) size in
  let+ args = list_size (return arity) (int_range (-20) 20) in
  let fn = List.fold_right (fun p b -> Ast.Abs (p, b)) params body in
  { name = Printf.sprintf "random %d" i; program = [ Ast.DLet ("f", fn) ]; args }
;;

(* ---------- running ---------- *)

let report case asm actual expected =
  Format.printf
    "FAIL %s: expected %d, got %s@.%s@.args: %s@.%a@.@."
    case.name
    expected
    actual
    (Printast.show_program case.program)
    (String.concat " " (List.map string_of_int case.args))
    Riscv.pp_instrs
    asm
;;

let check cases =
  (* Cases the interpreter rejects (division by zero) are skipped. *)
  let compiled =
    List.filter_map
      (fun case ->
         match expected case, compile ~name:"fN" case.program with
         | Some e, Ok _ -> Some (case, e)
         | None, _ -> None
         | Some _, Error msg ->
           Format.printf "FAIL %s: %s@." case.name msg;
           None)
      cases
  in
  let functions =
    List.mapi
      (fun i (case, _) ->
         ( Result.value (compile ~name:(Printf.sprintf "f%d" i) case.program) ~default:[]
         , case.args ))
      compiled
  in
  match run_on_qemu functions with
  | Error msg ->
    Format.printf "FAIL: %s@." msg;
    false
  | Ok lines when List.compare_lengths lines compiled <> 0 ->
    Format.printf
      "FAIL: %d results for %d cases@."
      (List.length lines)
      (List.length compiled);
    false
  | Ok lines ->
    let failures =
      List.filteri
        (fun i ((case, e), line) ->
           let ok = String.equal line (string_of_int e) in
           if not ok then report case (fst (List.nth functions i)) line e;
           not ok)
        (List.combine compiled lines)
    in
    Format.printf
      "%d cases run on QEMU (%d skipped), %d failed@."
      (List.length compiled)
      (List.length cases - List.length compiled)
      (List.length failures);
    List.is_empty failures
;;

let print_asm () =
  let program = parse (In_channel.input_all stdin) in
  List.iter
    (function
      | Anf.DLet (name, params, body) | Anf.DLetRec (name, params, body) ->
        (match Codegen.compile_function Codegen.no_calls ~name ~params body with
         | Ok code -> Format.printf "%a@.@." Riscv.pp_instrs code
         | Error msg -> Format.printf "%s: %s@." name msg)
      | Anf.DExpr _ -> ())
    (Anf.of_program program)
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; "asm" ] -> print_asm ()
  | _ when not (toolchain_available ()) ->
    print_endline "skipped: riscv64-linux-gnu-gcc or qemu-riscv64 not found"
  | _ ->
    let seed = Random.State.bits (Random.State.make_self_init ()) in
    Format.printf "random seed: %d@." seed;
    let rand = Random.State.make [| seed |] in
    let random = List.init 500 (fun i -> Gen.generate1 ~rand (gen_case i)) in
    if not (check (unit_cases @ random)) then exit 1
;;
