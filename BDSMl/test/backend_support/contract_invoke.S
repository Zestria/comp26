/* Test-only ABI adapter. test_invoke(code, argc, argv). */
    .text
    .globl test_invoke
    .type test_invoke, @function
test_invoke:
    addi sp, sp, -48
    sd ra, 40(sp)
    sd s0, 32(sp)
    sd s1, 24(sp)
    sd s2, 16(sp)
    sd s3, 8(sp)
    mv s0, sp
    mv s1, a0
    mv s2, a1
    mv s3, a2
    addi t0, s2, -8
    blez t0, .Ltest_registers
    slli t0, t0, 3
    addi t0, t0, 15
    andi t0, t0, -16
    sub sp, sp, t0
    li t0, 8
.Ltest_stack:
    slli t1, t0, 3
    add t1, s3, t1
    ld t2, 0(t1)
    addi t1, t0, -8
    slli t1, t1, 3
    add t1, sp, t1
    sd t2, 0(t1)
    addi t0, t0, 1
    bltu t0, s2, .Ltest_stack
.Ltest_registers:
    li t0, 0
    beq t0, s2, .Ltest_call
    ld a0, 0(s3)
    li t0, 1
    beq t0, s2, .Ltest_call
    ld a1, 8(s3)
    li t0, 2
    beq t0, s2, .Ltest_call
    ld a2, 16(s3)
    li t0, 3
    beq t0, s2, .Ltest_call
    ld a3, 24(s3)
    li t0, 4
    beq t0, s2, .Ltest_call
    ld a4, 32(s3)
    li t0, 5
    beq t0, s2, .Ltest_call
    ld a5, 40(s3)
    li t0, 6
    beq t0, s2, .Ltest_call
    ld a6, 48(s3)
    li t0, 7
    beq t0, s2, .Ltest_call
    ld a7, 56(s3)
.Ltest_call:
    jalr ra, s1, 0
    mv sp, s0
    ld ra, 40(sp)
    ld s0, 32(sp)
    ld s1, 24(sp)
    ld s2, 16(sp)
    ld s3, 8(sp)
    addi sp, sp, 48
    ret
    .size test_invoke, .-test_invoke
    .section .note.GNU-stack,"",@progbits
