[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml
open Riscv

let show program = Format.asprintf "%a" pp_program program
let instr i = Instr (i, None)

let expect_equal expected actual =
  if expected <> actual
  then (
    Printf.eprintf "ожидалось:\n%s\nполучено:\n%s\n" expected actual;
    exit 1)
;;

(* Печать: директивы и инструкции с отступом, метки без, комментарий после #. *)
let () =
  let main = label "vc_main" in
  expect_equal
    "    .globl vc_main\n\
     vc_main:\n\
    \    addi sp, sp, -16\n\
    \    sd ra, 8(sp)\n\
    \    li a0, 42  # ответ\n\
    \    call vc_print_int\n\
    \    beq a0, zero, vc_main\n\
    \    .asciz \"hi\\012\\\"\\303\\251\"\n"
    (show
       [ Directive (Globl main)
       ; Label main
       ; instr (Addi (Sp, Sp, -16))
       ; instr (Sd (Ra, 8, Sp))
       ; Instr (Li (A0, 42), Some "ответ")
       ; instr (Call (label "vc_print_int"))
       ; instr (Beq (A0, Zero, main))
       ; Directive (Asciz "hi\n\"\xc3\xa9")
       ])
;;

(* Свежие метки локальные и разные. *)
let () =
  Fresh.reset ();
  let first = fresh_label () in
  let second = fresh_label () in
  expect_equal ".L__1" (first :> string);
  assert (first <> second)
;;

(* Иммедиат вне 12 бит ловим при печати; li печатает любой. *)
let () =
  let fails program =
    match show program with
    | _ -> false
    | exception Assert_failure _ -> true
  in
  assert (fails [ instr (Addi (A0, A0, 2048)) ]);
  assert (fails [ instr (Ld (A0, -2049, Sp)) ]);
  assert (fails [ instr (Sd (A0, 4096, Sp)) ]);
  assert (not (fails [ instr (Addi (A0, A0, -2048)); instr (Li (A0, 1 lsl 40)) ]))
;;

let () =
  let check expected program = assert (peephole program = expected) in
  (* Пустые пересылки уходят. *)
  check [ instr Ret ] [ instr (Mv (A0, A0)); instr (Addi (T0, T0, 0)); instr Ret ];
  check [ instr (Addi (A0, T0, 0)) ] [ instr (Addi (A0, T0, 0)) ];
  (* ld; sd в тот же слот: запись лишняя. *)
  check [ instr (Ld (T0, 8, Sp)) ] [ instr (Ld (T0, 8, Sp)); instr (Sd (T0, 8, Sp)) ];
  check
    [ instr (Ld (T0, 8, Sp)); instr (Sd (T0, 16, Sp)) ]
    [ instr (Ld (T0, 8, Sp)); instr (Sd (T0, 16, Sp)) ];
  (* sd; ld из того же слота: читаем из регистра. *)
  check
    [ instr (Sd (T0, 8, Sp)); instr (Mv (T1, T0)) ]
    [ instr (Sd (T0, 8, Sp)); instr (Ld (T1, 8, Sp)) ];
  check [ instr (Sd (T0, 8, Sp)) ] [ instr (Sd (T0, 8, Sp)); instr (Ld (T0, 8, Sp)) ];
  (* Через другой базовый регистр не трогаем: адрес может отличаться. *)
  check
    [ instr (Sd (T0, 8, A0)); instr (Ld (T1, 8, A0)) ]
    [ instr (Sd (T0, 8, A0)); instr (Ld (T1, 8, A0)) ];
  (* Метка --- барьер: на неё могут прыгнуть с другим содержимым регистров. *)
  let l = label ".Lx" in
  let barrier = [ instr (Sd (T0, 8, Sp)); Label l; instr (Ld (T1, 8, Sp)) ] in
  check barrier barrier
;;
