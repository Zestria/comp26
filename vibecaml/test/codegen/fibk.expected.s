    .text
    .globl vc_fn_lifted__5_0
vc_fn_lifted__5_0:
    addi sp, sp, -64  # кадр: ra, fp, локальные и исходящие аргументы
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 64
    sd a0, -24(s0)
    sd a1, -32(s0)
    sd a2, -40(s0)
    ld t0, -24(s0)
    ld t1, -40(s0)
    add a0, t0, t1
    sd a0, -48(s0)
    ld t0, -48(s0)
    sd t0, 0(sp)
    ld a0, -32(s0)
    li a1, 1
    mv a2, sp
    call vc_apply
    ld ra, 56(sp)
    ld s0, 48(sp)
    addi sp, sp, 64
    ret
    .text
    .globl vc_fn_lifted__6_1
vc_fn_lifted__6_1:
    addi sp, sp, -80  # кадр: ra, fp, локальные и исходящие аргументы
    sd ra, 72(sp)
    sd s0, 64(sp)
    addi s0, sp, 80
    sd a0, -24(s0)
    sd a1, -32(s0)
    sd a2, -40(s0)
    ld t0, -40(s0)
    sd t0, 0(sp)
    ld t0, -24(s0)
    sd t0, 8(sp)
    la a0, vc_closure_lifted__5_0
    li a1, 2
    mv a2, sp
    call vc_apply
    sd a0, -48(s0)
    ld t0, -32(s0)
    li t1, 2
    sub a0, t0, t1
    sd a0, -56(s0)
    ld a0, -56(s0)
    ld a1, -48(s0)
    call vc_fn_fibk_2
    ld ra, 72(sp)
    ld s0, 64(sp)
    addi sp, sp, 80
    ret
    .text
    .globl vc_fn_fibk_2
vc_fn_fibk_2:
    addi sp, sp, -64  # кадр: ra, fp, локальные и исходящие аргументы
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 64
    sd a0, -24(s0)
    sd a1, -32(s0)
    ld t0, -24(s0)
    li t1, 2
    bge t0, t1, .L__21
    ld t0, -24(s0)
    sd t0, 0(sp)
    ld a0, -32(s0)
    li a1, 1
    mv a2, sp
    call vc_apply
    j .L__22
.L__21:
    ld t0, -32(s0)
    sd t0, 0(sp)
    ld t0, -24(s0)
    sd t0, 8(sp)
    la a0, vc_closure_lifted__6_1
    li a1, 2
    mv a2, sp
    call vc_apply
    sd a0, -40(s0)
    ld t0, -24(s0)
    li t1, 1
    sub a0, t0, t1
    sd a0, -48(s0)
    ld a0, -48(s0)
    ld a1, -40(s0)
    call vc_fn_fibk_2
.L__22:
    ld ra, 56(sp)
    ld s0, 48(sp)
    addi sp, sp, 64
    ret
    .text
    .globl vc_fn_lifted__7_3
vc_fn_lifted__7_3:
    addi sp, sp, -32  # кадр: ra, fp, локальные и исходящие аргументы
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 32
    sd a0, -24(s0)
    ld a0, -24(s0)
    ld ra, 24(sp)
    ld s0, 16(sp)
    addi sp, sp, 32
    ret
    .text
    .globl vc_main
vc_main:
    addi sp, sp, -48  # кадр: ra, fp, локальные и исходящие аргументы
    sd ra, 40(sp)
    sd s0, 32(sp)
    addi s0, sp, 48
    li a0, 10
    la a1, vc_closure_lifted__7_3
    call vc_fn_fibk_2
    sd a0, -24(s0)
    ld a0, -24(s0)
    call vc_print_int
    li a0, 0
    la t0, vc_value_value__18_4
    sd a0, 0(t0)
    li a0, 0
    ld ra, 40(sp)
    ld s0, 32(sp)
    addi sp, sp, 48
    ret
    .data
    .p2align 3
vc_value_value__18_4:
    .dword 0
vc_closure_lifted__5_0:
    .dword vc_fn_lifted__5_0
    .dword 3
    .dword 0
vc_closure_lifted__6_1:
    .dword vc_fn_lifted__6_1
    .dword 3
    .dword 0
vc_closure_lifted__7_3:
    .dword vc_fn_lifted__7_3
    .dword 1
    .dword 0
