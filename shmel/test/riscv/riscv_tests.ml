[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Code generation for function bodies, checked under QEMU: every case is a
   function [let f p1 ... pn = body], its result is compared with the
   reference interpreter. The functions may call [ext1] and [ext2], written in
   assembly so that they spoil every register a call may spoil. All the cases
   go into one assembly file, so QEMU starts once.

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

let parse source =
  match Main.parse_program_with_comb source with
  | Ok program -> program
  | Error (`Parsing_error msg) -> [ Ast.DExpr (Ast.Var ("parse error: " ^ msg)) ]
;;

(* The functions the cases may call, for the interpreter. *)
let externals = parse "let ext1 a = a + 1000\nlet ext2 a b = a * 31 + b"
let arities = [ "ext1", 1; "ext2", 2 ]

(* The same functions in assembly; afterwards every [t*] and [a*] but the
   result is -1. *)
let ext_functions =
  let spoil =
    List.map
      (fun r -> Riscv.Li (r, -1))
      (List.init 7 (fun i -> Riscv.T i) @ List.init 7 (fun i -> Riscv.A (i + 1)))
  in
  Riscv.([ Label "ext1"; I (Addi, A 0, A 0, 1000) ] @ spoil @ [ Ret ])
  @ Riscv.(
      [ Label "ext2"; Li (T 0, 31); R (Mul, A 0, A 0, T 0); R (Add, A 0, A 0, A 1) ]
      @ spoil
      @ [ Ret ])
;;

let all_ok results =
  List.fold_right
    (fun r acc ->
       match r, acc with
       | Ok x, Ok xs -> Ok (x :: xs)
       | Error e, _ | _, Error e -> Error e)
    results
    (Ok [])
;;

(* Calls of [ext1] and [ext2] with all their arguments, following psABI. *)
let hooks =
  { Codegen.load_global =
      (fun rd x ->
        if List.mem_assoc x arities
        then Ok [ Riscv.La (rd, x) ]
        else Error (Printf.sprintf "unbound variable %s" x))
  ; call =
      (fun ~load f args ->
        match f with
        | Anf.ImmVar name
          when Option.equal
                 Int.equal
                 (List.assoc_opt name arities)
                 (Some (List.length args)) ->
          Result.map
            (fun loads -> List.concat loads @ [ Riscv.Call name ])
            (all_ok (List.mapi (fun i a -> load (Riscv.A i) a) args))
        | _ -> Error "only full calls of ext1 and ext2 are supported in the tests")
  }
;;

let call_f args =
  Ast.DExpr (List.fold_left (fun f a -> Ast.App (f, Ast.Const a)) (Ast.Var "f") args)
;;

(* [None] when the program fails (division by zero) or the number is so big
   that 63-bit OCaml integers and 64-bit registers may disagree. *)
let expected case =
  match (Eval.run (externals @ case.program @ [ call_f case.args ])).result with
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
  | Some (params, body) -> Codegen.compile_function hooks ~name ~params body
;;

(* The caller side of psABI: 8 arguments in [a0] .. [a7], the rest on the
   stack. *)
let entry i args =
  let open Riscv in
  let in_regs = List.filteri (fun k _ -> k < 8) args in
  let on_stack = List.filteri (fun k _ -> k >= 8) args in
  let space = ((8 * List.length on_stack) + 15) / 16 * 16 in
  let adjust n = if n = 0 then [] else [ I (Addi, Sp, Sp, n) ] in
  [ Label (Printf.sprintf "entry%d" i); I (Addi, Sp, Sp, -16); Sd (Ra, 8, Sp) ]
  @ adjust (-space)
  @ List.concat (List.mapi (fun k a -> [ Li (T 0, a); Sd (T 0, 8 * k, Sp) ]) on_stack)
  @ List.mapi (fun k a -> Li (A k, a)) in_regs
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
let run_on_qemu ~docker functions =
  let code =
    ((Riscv.Directive ".text" :: ext_functions)
     @ List.concat (List.mapi (fun i (asm, args) -> asm @ entry i args) functions))
    @ table (List.length functions)
  in
  write_file "tests.s" (Format.asprintf "%a@." Riscv.pp_instrs code);
  let command =
    if docker
    then "sh docker_runner.sh codegen > tests.out"
    else "sh run_riscv.sh codegen > tests.out"
  in
  if Sys.command command = 0
  then Ok (In_channel.with_open_text "tests.out" In_channel.input_lines)
  else Error (Printf.sprintf "command failed: %s" command)
;;

(* ---------- hand-written cases ---------- *)

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
    ; "parameters in registers", "let f a b = b - a", [ 3; 10 ]
    ; "first parameter later", "let f a b = let x = b * 2 in x + a", [ 3; 10 ]
    ; ( "eight parameters"
      , "let f a b c d e g h i = a - b + c - d + e - g + h - i"
      , [ 1; 2; 3; 4; 5; 6; 7; 8 ] )
    ; ( "nine parameters"
      , "let f a b c d e g h i j = a + b * c - d + e * g - h + i * j"
      , [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] )
    ; ( "ten parameters"
      , "let f a b c d e g h i j k = j * 100 + k - a + i"
      , [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] )
    ; "shadowed parameter", "let f a b = let b = a + 1 in a * b", [ 4; 100 ]
    ; "call", "let f a = ext1 a + 1", [ 5 ]
    ; "tail call", "let f a = ext1 (a * 2)", [ 21 ]
    ; "parameter after call", "let f a b = let x = ext1 a in x + b", [ 5; 7 ]
    ; "first parameter after call", "let f a b = let x = ext1 b in x - a", [ 5; 7 ]
    ; "two arguments", "let f a b = ext2 b a", [ 2; 3 ]
    ; "nested calls", "let f a b = ext2 (ext1 a) (ext1 b)", [ 1; 2 ]
    ; "call in condition", "let f a = if ext1 a > 1005 then ext2 a a else a", [ 9 ]
    ; "call in one branch", "let f a b = if a < b then ext1 a + b else b - a", [ 1; 9 ]
    ; ( "call in the other branch"
      , "let f a b = if a < b then ext1 a + b else b - a"
      , [ 9; 1 ] )
    ; ( "variable across a call"
      , "let f a = let x = a * 3 in let y = ext1 x in x + y"
      , [ 4 ] )
    ; ( "parameter on the stack after call"
      , "let f a b c d e g h i j = let x = ext1 j in x + j + a"
      , [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] )
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
        ; 1, return `Call1
        ; 1, return `Call2
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
    | `Call1 -> map (fun a -> Ast.App (Ast.Var "ext1", a)) (smaller env)
    | `Call2 ->
      map2
        (fun a b -> Ast.App (Ast.App (Ast.Var "ext2", a), b))
        (smaller env)
        (smaller env)

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
  let* arity = oneof_weighted [ 6, int_range 0 3; 1, int_range 4 10 ] in
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

let check ~docker cases =
  (* Cases the interpreter rejects (division by zero) are skipped, and so are
     the ones where ANF reduced [fun p q -> ext2 p q] to [ext2]: [f] then has
     no parameters and cannot be called with the arguments. *)
  let arity_matches case =
    match function_of case.program with
    | Some (params, _) -> List.compare_lengths params case.args = 0
    | None -> true
  in
  let runnable =
    List.filter_map
      (fun c -> if arity_matches c then Option.map (fun e -> c, e) (expected c) else None)
      cases
  in
  let compiled =
    List.mapi
      (fun i (case, e) -> case, e, compile ~name:(Printf.sprintf "f%d" i) case.program)
      runnable
  in
  let errors =
    List.filter_map
      (fun (case, _, code) ->
         match code with
         | Ok _ -> None
         | Error msg -> Some (Format.asprintf "FAIL %s: %s" case.name msg))
      compiled
  in
  let functions =
    List.map (fun (case, _, code) -> Result.value code ~default:[], case.args) compiled
  in
  match errors, run_on_qemu ~docker functions with
  | _ :: _, _ ->
    List.iter print_endline errors;
    false
  | [], Error msg ->
    Format.printf "FAIL: %s@." msg;
    false
  | [], Ok lines when List.compare_lengths lines compiled <> 0 ->
    Format.printf
      "FAIL: %d results for %d cases@."
      (List.length lines)
      (List.length compiled);
    false
  | [], Ok lines ->
    let failures =
      List.filter
        (fun ((case, e, code), line) ->
           let ok = String.equal line (string_of_int e) in
           if not ok then report case (Result.value code ~default:[]) line e;
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
        (match Codegen.compile_function hooks ~name ~params body with
         | Ok code -> Format.printf "%a@.@." Riscv.pp_instrs code
         | Error msg -> Format.printf "%s: %s@." name msg)
      | Anf.DExpr _ -> ())
    (Anf.of_program program)
;;

let run ~docker =
  if (not docker) && not (toolchain_available ())
  then print_endline "skipped: riscv64-linux-gnu-gcc or qemu-riscv64 not found"
  else (
    let seed = Random.State.bits (Random.State.make_self_init ()) in
    Format.printf "random seed: %d@." seed;
    let rand = Random.State.make [| seed |] in
    let random = List.init 500 (fun i -> Gen.generate1 ~rand (gen_case i)) in
    if not (check ~docker (unit_cases @ random)) then exit 1)
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; "asm" ] -> print_asm ()
  | [ _ ] | [ _; "--runner"; "local" ] -> run ~docker:false
  | [ _; "--runner"; "docker" ] -> run ~docker:true
  | _ ->
    prerr_endline "usage: riscv_tests.exe [asm | --runner local|docker]";
    exit 2
;;
