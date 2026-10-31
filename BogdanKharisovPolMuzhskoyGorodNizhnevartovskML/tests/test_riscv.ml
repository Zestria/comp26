[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_backend

let check what condition = if not condition then failwith ("test_riscv: " ^ what)

let expect_invalid what f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith ("test_riscv: " ^ what ^ ": expected Invalid_argument")
;;

let expect_instrs what expected actual =
  check (what ^ ": unexpected instruction sequence") (expected = actual)
;;

let a0 = Riscv.A 0
let t0 = Riscv.T 0
let scratch = Riscv.T 6

let () =
  (* Immediate range: 12 signed bits. *)
  check "fits_imm12 -2048" (Riscv.fits_imm12 (-2048));
  check "fits_imm12 2047" (Riscv.fits_imm12 2047);
  check "fits_imm12 2048" (not (Riscv.fits_imm12 2048));
  check "fits_imm12 -2049" (not (Riscv.fits_imm12 (-2049)));
  check "fits_imm12 0" (Riscv.fits_imm12 0)
;;

let () =
  (* Scratch register. *)
  check "scratch is t6" (Riscv.scratch = Riscv.T 6);
  check "reg_to_string scratch" (Riscv.reg_to_string Riscv.scratch = "t6")
;;

let () =
  (* add_imm: one addi in range, li + add out of range. *)
  expect_instrs "add_imm 0" [ Riscv.Imm (Riscv.Addi, t0, a0, 0) ] (Riscv.add_imm t0 a0 0);
  expect_instrs
    "add_imm -2048"
    [ Riscv.Imm (Riscv.Addi, t0, a0, -2048) ]
    (Riscv.add_imm t0 a0 (-2048));
  expect_instrs
    "add_imm 2047"
    [ Riscv.Imm (Riscv.Addi, t0, a0, 2047) ]
    (Riscv.add_imm t0 a0 2047);
  expect_instrs
    "add_imm rd = rs"
    [ Riscv.Imm (Riscv.Addi, a0, a0, 7) ]
    (Riscv.add_imm a0 a0 7);
  expect_instrs
    "add_imm 2048"
    [ Riscv.Li (scratch, 2048L); Riscv.Binop (Riscv.Add, t0, a0, scratch) ]
    (Riscv.add_imm t0 a0 2048);
  expect_instrs
    "add_imm -2049"
    [ Riscv.Li (scratch, -2049L); Riscv.Binop (Riscv.Add, t0, a0, scratch) ]
    (Riscv.add_imm t0 a0 (-2049));
  expect_invalid "add_imm scratch rd" (fun () -> Riscv.add_imm scratch a0 5);
  expect_invalid "add_imm scratch rs" (fun () -> Riscv.add_imm t0 scratch 5)
;;

let () =
  (* load: one ld in range; out of range the offset is materialized in t6
     first: li t6, off; add t6, base, t6; ld rd, 0(t6).  rd and base are
     never t6 (rejected below), so writing t6 destroys no input and
     rd = base stays legal. *)
  expect_instrs "load 0" [ Riscv.Ld (t0, 0, a0) ] (Riscv.load t0 0 a0);
  expect_instrs "load 2047" [ Riscv.Ld (t0, 2047, a0) ] (Riscv.load t0 2047 a0);
  expect_instrs "load -2048" [ Riscv.Ld (t0, -2048, a0) ] (Riscv.load t0 (-2048) a0);
  expect_instrs "load rd = base" [ Riscv.Ld (a0, 8, a0) ] (Riscv.load a0 8 a0);
  expect_instrs
    "load 2048"
    [ Riscv.Li (scratch, 2048L)
    ; Riscv.Binop (Riscv.Add, scratch, a0, scratch)
    ; Riscv.Ld (t0, 0, scratch)
    ]
    (Riscv.load t0 2048 a0);
  expect_instrs
    "load -2049"
    [ Riscv.Li (scratch, -2049L)
    ; Riscv.Binop (Riscv.Add, scratch, a0, scratch)
    ; Riscv.Ld (t0, 0, scratch)
    ]
    (Riscv.load t0 (-2049) a0);
  expect_invalid "load scratch rd" (fun () -> Riscv.load scratch 0 a0);
  expect_invalid "load scratch base" (fun () -> Riscv.load t0 0 scratch)
;;

let () =
  (* store: legalized like load, src and base are read after t6. *)
  expect_instrs "store 16" [ Riscv.Sd (t0, 16, a0) ] (Riscv.store t0 16 a0);
  expect_instrs "store 2047" [ Riscv.Sd (t0, 2047, a0) ] (Riscv.store t0 2047 a0);
  expect_instrs "store -2048" [ Riscv.Sd (t0, -2048, a0) ] (Riscv.store t0 (-2048) a0);
  expect_instrs
    "store 2048"
    [ Riscv.Li (scratch, 2048L)
    ; Riscv.Binop (Riscv.Add, scratch, a0, scratch)
    ; Riscv.Sd (t0, 0, scratch)
    ]
    (Riscv.store t0 2048 a0);
  expect_invalid "store scratch src" (fun () -> Riscv.store scratch 0 a0);
  expect_invalid "store scratch base" (fun () -> Riscv.store t0 0 scratch)
;;

let () =
  (* Register names. *)
  check
    "reg_to_string"
    (Riscv.reg_to_string Riscv.Zero = "zero"
     && Riscv.reg_to_string Riscv.Ra = "ra"
     && Riscv.reg_to_string Riscv.Sp = "sp"
     && Riscv.reg_to_string Riscv.Fp = "s0"
     && Riscv.reg_to_string (Riscv.S 1) = "s1"
     && Riscv.reg_to_string (Riscv.S 11) = "s11"
     && Riscv.reg_to_string (Riscv.A 0) = "a0"
     && Riscv.reg_to_string (Riscv.A 7) = "a7"
     && Riscv.reg_to_string (Riscv.T 0) = "t0")
;;

let () =
  (* Assembly syntax: one instruction per line, labels unindented. *)
  check "to_string empty" (Riscv.to_string [] = "");
  check
    "to_string ld"
    (Riscv.to_string [ Riscv.Ld (Riscv.A 0, 16, Riscv.Sp) ] = "\tld\ta0, 16(sp)\n");
  check "to_string li" (Riscv.to_string [ Riscv.Li (Riscv.A 1, -1L) ] = "\tli\ta1, -1\n");
  check
    "to_string la"
    (Riscv.to_string [ Riscv.La (t0, "rt_apply") ] = "\tla\tt0, rt_apply\n");
  check "to_string mv" (Riscv.to_string [ Riscv.Mv (t0, a0) ] = "\tmv\tt0, a0\n");
  check
    "to_string binop"
    (Riscv.to_string [ Riscv.Binop (Riscv.Sub, t0, a0, Riscv.A 1) ]
     = "\tsub\tt0, a0, a1\n");
  check
    "to_string imm"
    (Riscv.to_string [ Riscv.Imm (Riscv.Slli, t0, a0, 1) ] = "\tslli\tt0, a0, 1\n");
  check "to_string jalr" (Riscv.to_string [ Riscv.Jalr t0 ] = "\tjalr\tra, 0(t0)\n");
  check
    "to_string call"
    (Riscv.to_string [ Riscv.Call "rt_print_int" ] = "\tcall\trt_print_int\n");
  check
    "to_string branch"
    (Riscv.to_string [ Riscv.Branch (Riscv.Blt, a0, Riscv.A 1, "L2") ]
     = "\tblt\ta0, a1, L2\n");
  check "to_string beqz" (Riscv.to_string [ Riscv.Beqz (a0, "L3") ] = "\tbeqz\ta0, L3\n");
  check
    "to_string directive"
    (Riscv.to_string [ Riscv.Directive ".globl ml_main" ] = "\t.globl ml_main\n");
  check "to_string comment" (Riscv.to_string [ Riscv.Comment "text" ] = "\t# text\n");
  check
    "to_string program"
    (Riscv.to_string [ Riscv.Label "ml_main"; Riscv.Li (a0, 1L); Riscv.Ret ]
     = "ml_main:\n\tli\ta0, 1\n\tret\n")
;;

let () =
  (* Data layout. *)
  check "abi word" (Abi.word = 8);
  check
    "abi tuple_field"
    (Abi.tuple_field 0 = 8 && Abi.tuple_field 1 = 16 && Abi.tuple_field 5 = 48);
  check "abi closure_code" (Abi.closure_code = 8);
  check "abi closure_arity" (Abi.closure_arity = 16);
  check
    "abi closure_env"
    (Abi.closure_env 0 = 24 && Abi.closure_env 1 = 32 && Abi.closure_env 7 = 80);
  check
    "abi tuple_header"
    (Abi.tuple_header 0 = 0L && Abi.tuple_header 3 = 6L && Abi.tuple_header 100 = 200L);
  check "abi closure_header" (Abi.closure_header = 1L);
  check "abi unit_value" (Abi.unit_value = 1L);
  check "abi bool_value" (Abi.bool_value false = 1L && Abi.bool_value true = 3L)
;;

let () =
  (* Runtime symbols. *)
  check
    "abi runtime symbols"
    (Abi.alloc = "rt_alloc"
     && Abi.apply = "rt_apply"
     && Abi.compare = "rt_compare"
     && Abi.print_int = "rt_print_int"
     && Abi.div_by_zero = "rt_div_by_zero");
  check "abi entry" (Abi.entry = "ml_main")
;;

let () = print_endline "test_riscv: OK"
