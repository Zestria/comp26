# При тестовой куче 48 байт частичное применение занимает её ровно целиком:
# 24 байта заголовка и три аргумента по 8 байт. Полное применение уже не
# выделяет память; следующая попытка выделения должна попасть в vc_fail.
.data
.p2align 3
heap_args: .dword 1, 2, 3, 4
heap_function: .dword sum_four, 4, 0
.text
sum_four:
    add a0, a0, a1
    add a0, a0, a2
    add a0, a0, a3
    ret
.globl vc_main
vc_main:
    addi sp, sp, -16
    sd ra, 8(sp)
    la a0, heap_function
    li a1, 3
    la a2, heap_args
    call vc_apply
    li a1, 1
    la a2, heap_args
    addi a2, a2, 24
    call vc_apply
    call vc_print_int
    call vc_print_newline
    la t0, vc_heap_end
    ld t0, 0(t0)
    sub a0, t0, s10
    call vc_print_int
    call vc_print_newline
    la a0, sum_four
    li a1, 4
    call vc_closure
    ld ra, 8(sp)
    addi sp, sp, 16
    ret
