/* Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

    .text
    .globl shmel_invoke_abi
    .type shmel_invoke_abi, @function
/* a0 = code, a1 = argc, a2 = argv. Inputs are checked by shmel_invoke. */
shmel_invoke_abi:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    sd s1, 8(sp)
    sd s2, 0(sp)
    mv s0, sp
    mv s1, a0
    mv s2, a2

    li t0, 8
    bleu a1, t0, .Lshmel_registers
    sub t0, a1, t0
    slli t0, t0, 3
    addi t0, t0, 15
    andi t0, t0, -16
    sub sp, sp, t0

    li t0, 8
    addi t1, s2, 64
    mv t2, sp
.Lshmel_stack_arguments:
    ld t3, 0(t1)
    sd t3, 0(t2)
    addi t0, t0, 1
    addi t1, t1, 8
    addi t2, t2, 8
    bltu t0, a1, .Lshmel_stack_arguments

.Lshmel_registers:
    mv t0, a1
    beqz t0, .Lshmel_call
    ld a0, 0(s2)
    addi t0, t0, -1
    beqz t0, .Lshmel_call
    ld a1, 8(s2)
    addi t0, t0, -1
    beqz t0, .Lshmel_call
    ld a2, 16(s2)
    addi t0, t0, -1
    beqz t0, .Lshmel_call
    ld a3, 24(s2)
    addi t0, t0, -1
    beqz t0, .Lshmel_call
    ld a4, 32(s2)
    addi t0, t0, -1
    beqz t0, .Lshmel_call
    ld a5, 40(s2)
    addi t0, t0, -1
    beqz t0, .Lshmel_call
    ld a6, 48(s2)
    addi t0, t0, -1
    beqz t0, .Lshmel_call
    ld a7, 56(s2)
.Lshmel_call:
    jalr ra, 0(s1)
    mv sp, s0
    ld s2, 0(sp)
    ld s1, 8(sp)
    ld s0, 16(sp)
    ld ra, 24(sp)
    addi sp, sp, 32
    ret
    .size shmel_invoke_abi, .-shmel_invoke_abi

    .section .note.GNU-stack,"",@progbits
