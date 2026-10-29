[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml
open Riscv

let show program = Format.asprintf "%a" pp_program program
let instr i = Instr (i, None)

let () =
  assert (
    show
      [ instr (And (A0, T0, T1))
      ; instr (Sra (A0, T0, T1))
      ; instr (Neg (A0, T0))
      ; Directive (Align 3)
      ; Directive (Dword_label (label "f"))
      ]
    = "    and a0, t0, t1\n\
      \    sra a0, t0, t1\n\
      \    neg a0, t0\n\
      \    .p2align 3\n\
      \    .dword f\n")
;;

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
  (* Если ld меняет sp, следующий sd уже пишет по другому адресу. *)
  let changes_base = [ instr (Ld (Sp, 8, Sp)); instr (Sd (Sp, 8, Sp)) ] in
  check changes_base changes_base;
  (* ld в zero отбрасывает значение, а sd zero обнуляет память.
     Это не запись загруженного слова обратно. *)
  List.iter
    (fun base ->
       let clears_slot = [ instr (Ld (Zero, 8, base)); instr (Sd (Zero, 8, base)) ] in
       check clears_slot clears_slot)
    [ Sp; S0; A0 ];
  check
    [ instr (Ld (T0, 8, Sp)); instr (Sd (T0, 16, Sp)) ]
    [ instr (Ld (T0, 8, Sp)); instr (Sd (T0, 16, Sp)) ];
  (* sd; ld из того же слота: читаем из регистра. *)
  check
    [ instr (Sd (T0, 8, Sp)); instr (Mv (T1, T0)) ]
    [ instr (Sd (T0, 8, Sp)); instr (Ld (T1, 8, Sp)) ];
  check [ instr (Sd (T0, 8, Sp)) ] [ instr (Sd (T0, 8, Sp)); instr (Ld (T0, 8, Sp)) ];
  (* Слоты кадра через s0 сокращаются так же, как через sp. *)
  check
    [ instr (Sd (A0, -48, S0)); instr (Mv (T0, A0)) ]
    [ instr (Sd (A0, -48, S0)); instr (Ld (T0, -48, S0)) ];
  check
    [ instr (Ld (T0, -24, S0)) ]
    [ instr (Ld (T0, -24, S0)); instr (Sd (T0, -24, S0)) ];
  (* Разные базы не трогаем: адреса могут отличаться. *)
  let other_base = [ instr (Sd (T0, 8, Sp)); instr (Ld (T1, 8, S0)) ] in
  check other_base other_base;
  (* Метка --- барьер: на неё могут прыгнуть с другим содержимым регистров. *)
  let l = label ".Lx" in
  let barrier = [ instr (Sd (T0, 8, Sp)); Label l; instr (Ld (T1, 8, Sp)) ] in
  check barrier barrier
;;

(* Сравниваем не вид списка инструкций, а регистры и память после исполнения.
   Маленькая модель покрывает только окно peephole; память обычная, не MMIO.
   В частности, запись в zero должна отбрасываться и в исходной программе,
   и в результате оптимизации. *)
let () =
  let registers = [ Zero; Sp; S0; A0; T0; T1 ] in
  let initial_registers = [ Sp, 16; S0, 32; A0, 40; T0, 48; T1, 56 ] in
  let initial_memory = List.init 10 (fun n -> 8 * n, 100 + n) in
  let get key values = Option.value ~default:0 (List.assoc_opt key values) in
  let set key value values =
    let rest = List.remove_assoc key values in
    if value = 0 then rest else (key, value) :: rest
  in
  let run program =
    let step (regs, memory) = function
      | Instr (instruction, _) ->
        let read register = if register = Zero then 0 else get register regs in
        let write register value =
          if register = Zero then regs else set register value regs
        in
        (match instruction with
         | Ld (rd, offset, base) -> write rd (get (read base + offset) memory), memory
         | Sd (rs, offset, base) -> regs, set (read base + offset) (read rs) memory
         | Mv (rd, rs) -> write rd (read rs), memory
         | Addi (rd, rs, offset) -> write rd (read rs + offset), memory
         | _ -> assert false)
      | Label _ | Directive _ -> assert false
    in
    let regs, memory = List.fold_left step (initial_registers, initial_memory) program in
    List.sort compare regs, List.sort compare memory
  in
  List.iter
    (fun base ->
       List.iter
         (fun base' ->
            List.iter
              (fun rd ->
                 List.iter
                   (fun rs ->
                      List.iter
                        (fun offset ->
                           let load = instr (Ld (rd, 8, base)) in
                           let store = instr (Sd (rs, offset, base')) in
                           List.iter
                             (fun program ->
                                assert (run program = run (peephole program)))
                             [ [ load; store ]; [ store; load ] ])
                        [ 8; 16 ])
                   registers)
              registers)
         registers)
    registers
;;
