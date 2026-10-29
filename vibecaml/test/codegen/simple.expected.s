    .text
    .globl vc_fn_choose_0
vc_fn_choose_0:
    addi sp, sp, -64  # кадр: ra, fp, локальные и исходящие аргументы
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 64
    sd a0, -24(s0)
    sd a1, -32(s0)
    ld t0, -24(s0)
    beq t0, zero, .L__12
    ld t0, -32(s0)
    li t1, 2
    mul a0, t0, t1
    sd a0, -40(s0)
    j .L__13
.L__12:
    ld t0, -32(s0)
    li t1, 1
    sub a0, t0, t1
    sd a0, -40(s0)
.L__13:
    ld t0, -40(s0)
    li t1, 3
    add a0, t0, t1
    ld ra, 56(sp)
    ld s0, 48(sp)
    addi sp, sp, 64
    ret
    .text
    .globl vc_main
vc_main:
    addi sp, sp, -64  # кадр: ra, fp, локальные и исходящие аргументы
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 64
    li a0, 0
    li a1, 4
    call vc_fn_choose_0
    sd a0, -24(s0)
    li a0, 1
    li a1, 4
    call vc_fn_choose_0
    sd a0, -32(s0)
    mv a0, s10
    addi t1, s10, 16
    la t2, vc_heap_end
    ld t2, 0(t2)
    bge t2, t1, .L__14
    li a0, 1
    call vc_fail
.L__14:
    mv s10, t1
    ld t0, -32(s0)
    sd t0, 0(a0)
    ld t0, -24(s0)
    sd t0, 8(a0)
    la t0, vc_value_pair_1
    sd a0, 0(t0)
    la a0, vc_value_pair_1
    ld a0, 0(a0)
    la t0, vc_value_value__8_2
    sd a0, 0(t0)
    la t0, vc_value_value__8_2
    ld t0, 0(t0)
    ld a0, 0(t0)
    la t0, vc_value_left_3
    sd a0, 0(t0)
    la t0, vc_value_value__8_2
    ld t0, 0(t0)
    ld a0, 8(t0)
    la t0, vc_value_right_4
    sd a0, 0(t0)
    la t0, vc_value_left_3
    ld t0, 0(t0)
    la t1, vc_value_right_4
    ld t1, 0(t1)
    add a0, t0, t1
    sd a0, -40(s0)
    call vc_print_int
    la t0, vc_value_value__9_5
    sd a0, 0(t0)
    li a0, 0
    ld ra, 56(sp)
    ld s0, 48(sp)
    addi sp, sp, 64
    ret
    .data
    .p2align 3
vc_value_pair_1:
    .dword 0
vc_value_value__8_2:
    .dword 0
vc_value_left_3:
    .dword 0
vc_value_right_4:
    .dword 0
vc_value_value__9_5:
    .dword 0
