    .text
    .globl vc_fn_lifted__3_0
vc_fn_lifted__3_0:
    addi sp, sp, -64  # кадр: ra, fp, локальные и исходящие аргументы
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 64
    sd a0, -24(s0)
    sd a1, -32(s0)
    sd a2, -40(s0)
    ld t0, -40(s0)
    ld t1, -32(s0)
    mul a0, t0, t1
    sd a0, -48(s0)
    ld t0, -48(s0)
    sd t0, 0(sp)
    ld a0, -24(s0)
    li a1, 1
    mv a2, sp
    call vc_apply
    ld ra, 56(sp)
    ld s0, 48(sp)
    addi sp, sp, 64
    ret
    .text
    .globl vc_fn_fack_1
vc_fn_fack_1:
    addi sp, sp, -64  # кадр: ra, fp, локальные и исходящие аргументы
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 64
    sd a0, -24(s0)
    sd a1, -32(s0)
    ld t0, -24(s0)
    li t1, 1
    blt t1, t0, .L__15
    li t0, 1
    sd t0, 0(sp)
    ld a0, -32(s0)
    li a1, 1
    mv a2, sp
    call vc_apply
    j .L__16
.L__15:
    ld t0, -32(s0)
    sd t0, 0(sp)
    ld t0, -24(s0)
    sd t0, 8(sp)
    la a0, vc_closure_lifted__3_0
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
    call vc_fn_fack_1
.L__16:
    ld ra, 56(sp)
    ld s0, 48(sp)
    addi sp, sp, 64
    ret
    .text
    .globl vc_fn_lifted__4_2
vc_fn_lifted__4_2:
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
    li a0, 5
    la a1, vc_closure_lifted__4_2
    call vc_fn_fack_1
    sd a0, -24(s0)
    ld a0, -24(s0)
    call vc_print_int
    li a0, 0
    la t0, vc_value_value__12_3
    sd a0, 0(t0)
    li a0, 0
    ld ra, 40(sp)
    ld s0, 32(sp)
    addi sp, sp, 48
    ret
    .data
    .p2align 3
vc_value_value__12_3:
    .dword 0
vc_closure_lifted__3_0:
    .dword vc_fn_lifted__3_0
    .dword 3
    .dword 0
vc_closure_lifted__4_2:
    .dword vc_fn_lifted__4_2
    .dword 1
    .dword 0
