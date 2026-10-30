[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_lib
open Shmel_middleend
open Shmel_backend
module Env = Map.Make (String)

let get = function
  | Ok x -> x
  | Error e -> failwith e
;;

let parse source =
  match Main.parse_program_with_comb source with
  | Ok p -> p
  | Error (`Parsing_error message) -> failwith (message ^ "\n" ^ source)
;;

let slot label =
  Riscv.
    [ Directive ".data"
    ; Directive ".balign 8"
    ; Directive (".globl " ^ label)
    ; Label label
    ; Directive ".dword 0"
    ]
;;

(* Only a test fixture for part 3: globals are initialized by the C harness.
   Production code uses Calls.hooks without depending on this fixture. *)
let load_global globals rd name =
  match Env.find_opt name globals with
  | Some label -> Ok Riscv.[ La (rd, label); Ld (rd, 0, rd) ]
  | None -> Error ("unknown integration-test global " ^ name)
;;

type compiled =
  { name : string
  ; assembly : Riscv.instr list
  ; declarations : string list
  ; initialize : string list
  ; expected : string
  }

let compile_case case_id (name, source) =
  let program = parse source in
  let outcome = Eval.run program in
  let value =
    match outcome.result with
    | Ok (Eval.VInt n) -> n
    | Ok (Eval.VBool b) -> Bool.to_int b
    | Ok Eval.VUnit -> 0
    | Ok (Eval.VClosure _ | Eval.VBuiltin _) -> failwith (name ^ ": function result")
    | Error e -> failwith (Format.asprintf "%s: %a" name Eval.pp_error e)
  in
  let lifted =
    program
    |> Anf.of_program
    |> Closure_conversion.of_program
    |> Lambda_lifting.of_program
  in
  let builtin = Printf.sprintf "it_%d_print" case_id in
  let rec compile index globals asm declarations initialize last = function
    | [] ->
      let result = Option.get last in
      { name
      ; assembly = List.concat (List.rev asm)
      ; declarations = List.rev declarations
      ; initialize =
          List.rev initialize
          @ [ Printf.sprintf "printf(\"|%%lld\\n\", (long long)(int64_t)%s);" result ]
      ; expected = outcome.output ^ "|" ^ Int.to_string value
      }
    | decl :: rest ->
      let x, params, body, recursive =
        match decl with
        | Anf.DLet (x, params, body) -> x, params, body, false
        | Anf.DLetRec (x, params, body) -> x, params, body, true
        | Anf.DExpr _ -> failwith "lifting left a top-level expression"
      in
      let global = Printf.sprintf "it_%d_slot_%d" case_id index in
      let code = Printf.sprintf "it_%d_code_%d" case_id index in
      let body_globals = if recursive then Env.add x global globals else globals in
      let hooks = Calls.hooks ~load_global:(load_global body_globals) in
      let instructions = get (Codegen.compile_function hooks ~name:code ~params body) in
      let declaration =
        Printf.sprintf "extern shmel_word %s; extern shmel_word %s(void);" global code
      in
      let init =
        if List.is_empty params
        then Printf.sprintf "%s = %s();" global code
        else
          Printf.sprintf
            "%s = (shmel_word)(uintptr_t)shmel_make_closure(%s, %d);"
            global
            code
            (List.length params)
      in
      let assembly =
        slot global
        @ Riscv.[ Directive ".text"; Directive (".globl " ^ code) ]
        @ instructions
      in
      compile
        (index + 1)
        (Env.add x global globals)
        (assembly :: asm)
        (declaration :: declarations)
        (init :: initialize)
        (Some global)
        rest
  in
  compile
    0
    (Env.singleton "print_int" builtin)
    [ slot builtin ]
    [ Printf.sprintf "extern shmel_word %s;" builtin ]
    [ Printf.sprintf
        "%s = (shmel_word)(uintptr_t)shmel_make_closure((shmel_code_address)test_print, \
         1);"
        builtin
    ]
    None
    lifted
;;

let many_arguments ~partial =
  let params = List.init Closure.max_arity (Printf.sprintf "p%d") in
  let args = List.init Closure.max_arity Int.to_string in
  let call =
    if partial
    then
      Printf.sprintf
        "let g = f %s in g %s"
        (String.concat " " (List.take (Closure.max_arity - 1) args))
        (String.concat " " (List.drop (Closure.max_arity - 1) args))
    else "f " ^ String.concat " " args
  in
  Printf.sprintf
    "let f %s = p0 + p3 + p%d in %s"
    (String.concat " " params)
    (Closure.max_arity - 1)
    call
;;

(* A large application buffer need not imply a large code arity: each unary
   function returns the next one. This keeps the >12-bit-offset coverage. *)
let large_overapplication ~partial =
  let definitions =
    List.init 300 (fun i ->
      let index = 299 - i in
      if index = 299
      then "let f299 x = x"
      else Printf.sprintf "let f%d x = f%d" index (index + 1))
  in
  let args = List.init 300 Int.to_string in
  let call =
    if partial
    then
      Printf.sprintf
        "let g = f0 %s in g %s"
        (String.concat " " (List.take 260 args))
        (String.concat " " (List.drop 260 args))
    else "f0 " ^ String.concat " " args
  in
  String.concat "\n" (definitions @ [ "let result = " ^ call ])
;;

let cases =
  [ "simple call", "let f x = x + 1 in f 7"
  ; "argument order", "let f a b = a - b in f 17 5"
  ; "boolean result", "let less a b = a < b in less 2 3"
  ; "capture", "let x = 10 in let f y = x + y in f 7"
  ; ( "independent closures"
    , "let make x = fun y -> x + y in let a = make 10 in let b = make 20 in a 1 + b 2" )
  ; ( "nested captures"
    , "let make x = fun y -> fun z -> x + y + z in let a = make 10 in let b = a 20 in b 3"
    )
  ; ( "partial applications"
    , "let f a b c = a * 100 + b * 10 + c in let g = f 1 in let h = g 2 in h 3" )
  ; ( "overapplication"
    , "let f x = if x = 0 then (fun y -> y + 1) else (fun y -> y + 2) in f 0 7" )
  ; ( "chained overapplication"
    , "let f x = if x = 0 then (fun y -> if y = 0 then (fun z -> z + 10) else (fun z -> \
       z + 20)) else (fun y -> fun z -> y + z) in f 0 0 7" )
  ; ( "function parameter"
    , "let apply f x = f x in let add x y = x + y in let g = add 10 in apply g 7" )
  ; ( "function selected by if"
    , "let a x = x + 1 in let b x = x - 1 in let choose n = if n < 0 then a else b in \
       let f = choose 2 in f 7" )
  ; ( "locals survive calls"
    , "let f x = x + 1 in let g a b c = let x = f a in let y = f b in x + y + c in g 10 \
       20 30" )
  ; ( "parameters survive branch calls"
    , "let f x = x + 2 in let g a b = let x = if a < 0 then f a else f b in x + a + b in \
       g 3 4" )
  ; "negative values", "let f a b = a - b in let x = f 4 9 in f x 7"
  ; "large values", "let f x = x + 1099511627776 in f 7"
  ; "recursive function", "let rec fac n = if n <= 1 then 1 else n * fac (n - 1) in fac 8"
  ; ( "recursive capture"
    , "let x = 3 in let rec f n = if n = 0 then x else f (n - 1) + 1 in f 7" )
  ; ( "escaping recursive closure"
    , "let make x = let rec f n = if n = 0 then x else f (n - 1) + 1 in f in let g = \
       make 3 in g 7" )
  ; ( "recursion through value"
    , "let rec f n = if n <= 0 then 0 else let g = f in 1 + g (n - 1) in f 7" )
  ; ( "CPS factorial"
    , "let rec fac n k = if n <= 1 then k 1 else fac (n - 1) (fun r -> k (n * r)) in fac \
       8 (fun x -> x)" )
  ; ( "CPS Fibonacci"
    , "let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b \
       -> k (a + b))) in fib 10 (fun x -> x)" )
  ; ( "global values and shadowing"
    , "let x = 3\nlet f y = x + y\nlet x = 20\nlet result = f x" )
  ; ( "top-level recursion"
    , "let rec f n = if n <= 0 then 0 else n + f (n - 1)\nlet result = f 10" )
  ; ( "argument evaluation order"
    , "let emit x = let _ = print_int x in x in let add a b = a + b in add (emit 1) \
       (emit 2)" )
  ; "print as function value", "let run f x = f x in let _ = run print_int 42 in 7"
  ; "unit result", "let f x = print_int x in f 7"
  ; ( "eight arguments"
    , "let f a b c d e f g h = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h in let p = f \
       1 2 3 in p 4 5 6 7 8" )
  ; ( "capture and parameters survive calls"
    , "let bump x = x + 1 in let f a b c d e f g = let v = bump (a + g) in v + b + c + d \
       + e + f in f 1 2 3 4 5 6 7" )
  ; "maximum code arity", many_arguments ~partial:false
  ; "maximum partial code arity", many_arguments ~partial:true
  ; "large argument buffer", large_overapplication ~partial:false
  ; "split large application", large_overapplication ~partial:true
  ]
;;

let random_cases rand =
  let open QCheck.Gen in
  let generator =
    let* a = int_range (-30) 30 in
    let* b = int_range (-30) 30 in
    let* x = int_range (-30) 30 in
    let+ m = int_range (-5) 5 in
    Printf.sprintf
      "let make a = fun b -> a * (%d) + b in let f = make (%d) in let g = make (%d) in f \
       (%d) - g (%d)"
      m
      a
      b
      x
      a
  in
  List.init 100 (fun i ->
    Printf.sprintf "random closures %d" i, generate1 ~rand generator)
;;

let write_file path text =
  Out_channel.with_open_text path (fun oc -> output_string oc text)
;;

let run ~docker =
  let seed = Random.State.bits (Random.State.make_self_init ()) in
  Printf.printf "integration random seed: %d\n%!" seed;
  let compiled =
    List.mapi compile_case (cases @ random_cases (Random.State.make [| seed |]))
  in
  let assembly = List.concat_map (fun c -> c.assembly) compiled in
  write_file "integration.s" (Format.asprintf "%a@." Riscv.pp_instrs assembly);
  let declarations = List.concat_map (fun c -> c.declarations) compiled in
  let initialize = List.concat_map (fun c -> c.initialize) compiled in
  write_file
    "integration.c"
    (String.concat
       "\n"
       ([ "#include \"closure_runtime.h\""
        ; "#include <stdio.h>"
        ; "static shmel_word test_print(shmel_word n) { printf(\"%lld\", (long \
           long)(int64_t)n); return 0; }"
        ]
        @ declarations
        @ [ "int main(void) {" ]
        @ initialize
        @ [ "return 0; }\n" ]));
  let available = Sys.command "sh run_riscv.sh available >/dev/null 2>&1" = 0 in
  if (not docker) && not available
  then print_endline "skipped integration execution: cross-compiler or QEMU not found"
  else (
    let command =
      if docker
      then "sh docker_runner.sh integration > integration.out"
      else "sh run_riscv.sh integration ../../backend > integration.out"
    in
    if Sys.command command <> 0 then failwith ("command failed: " ^ command);
    let actual = In_channel.with_open_text "integration.out" In_channel.input_lines in
    if List.compare_lengths actual compiled <> 0
    then failwith "integration output has the wrong number of results";
    List.iter2
      (fun c actual ->
         if not (String.equal c.expected actual)
         then failwith (Printf.sprintf "%s: expected %S, got %S" c.name c.expected actual))
      compiled
      actual;
    Printf.printf
      "%d closure integration cases run on QEMU, 0 failed\n"
      (List.length compiled))
;;

let () =
  match Array.to_list Sys.argv with
  | [ _ ] | [ _; "--runner"; "local" ] -> run ~docker:false
  | [ _; "--runner"; "docker" ] -> run ~docker:true
  | _ ->
    prerr_endline "usage: calls_integration_tests.exe [--runner local|docker]";
    exit 2
;;
