[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Backend.Asm

let compile source =
  let externals =
    List.map Middleend.Anf.builtin_name Middleend.Anf.builtins @ [ "print_int" ]
  in
  match Topdown_parser.parse_program source with
  | Error error -> failwith (Error_monad.show_error error)
  | Ok program ->
    let program = Middleend.Lambda_lifting.lift_program program in
    (match Middleend.Closure_conversion.convert ~externals program with
     | Error error ->
       failwith (Format.asprintf "%a" Middleend.Closure_conversion.pp_error error)
     | Ok program ->
       (match Middleend.Normalize.program program with
        | Error error ->
          failwith (Format.asprintf "%a" Middleend.Normalize.pp_error error)
        | Ok anf -> Backend.Asm_conversion.compile anf))
;;

let check name source expected =
  let actual = compile source in
  if actual <> expected
  then
    failwith
      (Printf.sprintf
         "%s: generated asm differs@\n--- actual ---@\n%s@\n--- expected ---@\n%s"
         name
         (Backend.Codegen.string_of_program actual)
         (Backend.Codegen.string_of_program expected))
;;

let definition name exported body = { name; exported; body }

let test_constant () =
  check
    "constant definition"
    "let x = 1"
    [ definition
        "mmml_program_main"
        true
        [ Addi (Sp, Sp, -16L)
        ; Sd (Ra, 8L, Sp)
        ; Li (A0, 1L)
        ; Call "mmml_int"
        ; Sd (A0, 0L, Sp)
        ; Li (A0, 10L)
        ; Jump "main_exit1"
        ; Label "main_exit1"
        ; Ld (Ra, 8L, Sp)
        ; Addi (Sp, Sp, 16L)
        ; Ret
        ]
    ]
;;

let test_lambda () =
  check
    "identity lambda becomes a closure"
    "let f x = x"
    [ definition
        "mmml_program_main"
        true
        [ Addi (Sp, Sp, -16L)
        ; Sd (Ra, 8L, Sp)
        ; La (A0, "code1")
        ; Li (A1, 1L)
        ; Call "mmml_make_closure"
        ; Sd (A0, 0L, Sp)
        ; Li (A0, 10L)
        ; Jump "main_exit3"
        ; Label "main_exit3"
        ; Ld (Ra, 8L, Sp)
        ; Addi (Sp, Sp, 16L)
        ; Ret
        ]
    ; definition
        "code1"
        false
        [ Addi (Sp, Sp, -16L)
        ; Sd (Ra, 8L, Sp)
        ; Ld (T0, 0L, A0)
        ; Sd (T0, 0L, Sp)
        ; Ld (A0, 0L, Sp)
        ; Jump "exit2"
        ; Label "exit2"
        ; Ld (Ra, 8L, Sp)
        ; Addi (Sp, Sp, 16L)
        ; Ret
        ]
    ]
;;

let test_addition () =
  check
    "boxed addition"
    "let f x = x + 1"
    [ definition
        "mmml_program_main"
        true
        [ Addi (Sp, Sp, -16L)
        ; Sd (Ra, 8L, Sp)
        ; La (A0, "code1")
        ; Li (A1, 1L)
        ; Call "mmml_make_closure"
        ; Sd (A0, 0L, Sp)
        ; Li (A0, 10L)
        ; Jump "main_exit3"
        ; Label "main_exit3"
        ; Ld (Ra, 8L, Sp)
        ; Addi (Sp, Sp, 16L)
        ; Ret
        ]
    ; definition
        "code1"
        false
        [ Addi (Sp, Sp, -48L)
        ; Sd (Ra, 32L, Sp)
        ; Ld (T0, 0L, A0)
        ; Sd (T0, 0L, Sp)
        ; Ld (A0, 0L, Sp)
        ; Sd (A0, 8L, Sp)
        ; Li (A0, 1L)
        ; Call "mmml_int"
        ; Sd (A0, 16L, Sp)
        ; Ld (A0, 8L, Sp)
        ; Call "mmml_unbox_int"
        ; Sd (A0, 24L, Sp)
        ; Ld (A0, 16L, Sp)
        ; Call "mmml_unbox_int"
        ; Addi (A1, A0, 0L)
        ; Ld (A0, 24L, Sp)
        ; Binop (Add, A0, A0, A1)
        ; Call "mmml_int"
        ; Jump "exit2"
        ; Label "exit2"
        ; Ld (Ra, 32L, Sp)
        ; Addi (Sp, Sp, 48L)
        ; Ret
        ]
    ]
;;

let test_print_int () =
  check
    "print_int prints an integer and returns it"
    "let _ = print_int 5"
    [ definition
        "mmml_program_main"
        true
        [ Addi (Sp, Sp, -16L)
        ; Sd (Ra, 8L, Sp)
        ; Li (A0, 5L)
        ; Call "mmml_int"
        ; Call "mmml_print_int"
        ; Sd (A0, 0L, Sp)
        ; Li (A0, 10L)
        ; Jump "main_exit1"
        ; Label "main_exit1"
        ; Ld (Ra, 8L, Sp)
        ; Addi (Sp, Sp, 16L)
        ; Ret
        ]
    ]
;;

let test_print_int_shadowed () =
  check
    "a user binding may shadow print_int"
    "let print_int x = x"
    [ definition
        "mmml_program_main"
        true
        [ Addi (Sp, Sp, -16L)
        ; Sd (Ra, 8L, Sp)
        ; La (A0, "code1")
        ; Li (A1, 1L)
        ; Call "mmml_make_closure"
        ; Sd (A0, 0L, Sp)
        ; Li (A0, 10L)
        ; Jump "main_exit3"
        ; Label "main_exit3"
        ; Ld (Ra, 8L, Sp)
        ; Addi (Sp, Sp, 16L)
        ; Ret
        ]
    ; definition
        "code1"
        false
        [ Addi (Sp, Sp, -16L)
        ; Sd (Ra, 8L, Sp)
        ; Ld (T0, 0L, A0)
        ; Sd (T0, 0L, Sp)
        ; Ld (A0, 0L, Sp)
        ; Jump "exit2"
        ; Label "exit2"
        ; Ld (Ra, 8L, Sp)
        ; Addi (Sp, Sp, 16L)
        ; Ret
        ]
    ]
;;

let () =
  test_constant ();
  test_lambda ();
  test_addition ();
  test_print_int ();
  test_print_int_shadowed ();
  print_endline "asm_ast: ok"
;;
