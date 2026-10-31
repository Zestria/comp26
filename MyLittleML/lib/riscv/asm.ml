[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** RISC-V 64 assembly representation and emission (Task 3).

    The instruction set is what the code generator actually needs: base
    integer instructions, the M extension ([mul]/[div]/[rem]) and a few
    pseudo-instructions ([li], [mv], [neg], [seqz], [la], [j]).  All memory
    operations are 64-bit ([ld]/[sd]) — the backend targets
    [riscv64-unknown-linux-gnu] and every value is a raw 64-bit integer.

    {!emit} turns a program into GNU-assembler text ready for
    [riscv64-linux-gnu-gcc -c] / [as]. *)

module Reg = Program.Reg

type instr =
  (* constants / moves *)
  | Li of Reg.t * int
  | Mov of Reg.t * Reg.t
  | Neg of Reg.t * Reg.t
  (* arithmetic *)
  | Add of Reg.t * Reg.t * Reg.t
  | Addi of Reg.t * Reg.t * int
  | Sub of Reg.t * Reg.t * Reg.t
  | Mul of Reg.t * Reg.t * Reg.t
  | Div of Reg.t * Reg.t * Reg.t
  | Rem of Reg.t * Reg.t * Reg.t
  (* comparisons *)
  | Slt of Reg.t * Reg.t * Reg.t
  | Seqz of Reg.t * Reg.t
  | Xori of Reg.t * Reg.t * int
  (* branches *)
  | Beq of Reg.t * Reg.t * string
  | Bne of Reg.t * Reg.t * string
  | Blt of Reg.t * Reg.t * string
  | Bge of Reg.t * Reg.t * string
  | J of string
  (* calls: [Jal] for internal labels, [Call] for external symbols like
     [malloc] (assembler lowers it to auipc+jalr) *)
  | Jal of string
  | Call of string
  | Jalr of Reg.t
  | La of Reg.t * string
  | Ret
  (* memory (64-bit) *)
  | Ld of Reg.t * int * Reg.t
  | Sd of Reg.t * int * Reg.t
  | Sb of Reg.t * int * Reg.t
  (* system *)
  | Ecall
  (* structure *)
  | Label of string

(** An assembly file: instruction text plus read-only data.  [data] holds
    static closure records as [(label, quad-target)] pairs, emitted in the
    [.rodata] section as [label: .quad target]. *)
type t =
  { text : instr list
  ; data : (string * string) list
  }

let instr_to_string = function
  | Li (r, c) -> Format.sprintf "li %s, %d" (Reg.name r) c
  | Mov (dst, src) -> Format.sprintf "mv %s, %s" (Reg.name dst) (Reg.name src)
  | Neg (dst, src) -> Format.sprintf "neg %s, %s" (Reg.name dst) (Reg.name src)
  | Add (dst, a, b) ->
    Format.sprintf "add %s, %s, %s" (Reg.name dst) (Reg.name a) (Reg.name b)
  | Addi (dst, a, c) -> Format.sprintf "addi %s, %s, %d" (Reg.name dst) (Reg.name a) c
  | Sub (dst, a, b) ->
    Format.sprintf "sub %s, %s, %s" (Reg.name dst) (Reg.name a) (Reg.name b)
  | Mul (dst, a, b) ->
    Format.sprintf "mul %s, %s, %s" (Reg.name dst) (Reg.name a) (Reg.name b)
  | Div (dst, a, b) ->
    Format.sprintf "div %s, %s, %s" (Reg.name dst) (Reg.name a) (Reg.name b)
  | Rem (dst, a, b) ->
    Format.sprintf "rem %s, %s, %s" (Reg.name dst) (Reg.name a) (Reg.name b)
  | Slt (dst, a, b) ->
    Format.sprintf "slt %s, %s, %s" (Reg.name dst) (Reg.name a) (Reg.name b)
  | Seqz (dst, a) -> Format.sprintf "seqz %s, %s" (Reg.name dst) (Reg.name a)
  | Xori (dst, a, c) -> Format.sprintf "xori %s, %s, %d" (Reg.name dst) (Reg.name a) c
  | Beq (a, b, l) -> Format.sprintf "beq %s, %s, %s" (Reg.name a) (Reg.name b) l
  | Bne (a, b, l) -> Format.sprintf "bne %s, %s, %s" (Reg.name a) (Reg.name b) l
  | Blt (a, b, l) -> Format.sprintf "blt %s, %s, %s" (Reg.name a) (Reg.name b) l
  | Bge (a, b, l) -> Format.sprintf "bge %s, %s, %s" (Reg.name a) (Reg.name b) l
  | J l -> Format.sprintf "j %s" l
  | Jal l -> Format.sprintf "jal %s" l
  | Call l -> Format.sprintf "call %s" l
  | Jalr r -> Format.sprintf "jalr %s" (Reg.name r)
  | La (r, l) -> Format.sprintf "la %s, %s" (Reg.name r) l
  | Ret -> "ret"
  | Ld (dst, off, base) ->
    Format.sprintf "ld %s, %d(%s)" (Reg.name dst) off (Reg.name base)
  | Sd (src, off, base) ->
    Format.sprintf "sd %s, %d(%s)" (Reg.name src) off (Reg.name base)
  | Sb (src, off, base) ->
    Format.sprintf "sb %s, %d(%s)" (Reg.name src) off (Reg.name base)
  | Ecall -> "ecall"
  | Label l -> Format.sprintf "%s:" l
;;

(** Emit GNU-assembler text.  The [.text] section header is always present;
    [.globl main] is added when a [main] label exists so that the output
    links against the C runtime and runs under [qemu-riscv64]. *)
let emit { text; data } : string =
  let buf = Buffer.create 4096 in
  let out fmt = Format.kasprintf (Buffer.add_string buf) fmt in
  out ".text\n";
  if
    List.exists
      (function
        | Label "main" -> true
        | _ -> false)
      text
  then out ".globl main\n";
  Buffer.add_char buf '\n';
  List.iter
    (function
      | Label l -> out "%s:\n" l
      | i -> out "  %s\n" (instr_to_string i))
    text;
  if data <> []
  then (
    out "\n.section .rodata\n.align 3\n";
    List.iter (fun (l, target) -> out "%s:\n  .quad %s\n" l target) data);
  Buffer.contents buf
;;
