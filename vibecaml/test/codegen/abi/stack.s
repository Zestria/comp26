.data
.p2align 3
args: .dword 1,2,3,4,5,6,7,8,9
one: .dword probe,1,0
three: .dword probe,3,0
nine: .dword ninth,9,0
maker: .dword return_probe,1,0
.text
probe:
    andi a0, sp, 15
    ret
ninth:
    andi t0, sp, 15
    bnez t0, bad_stack
    ld a0, 0(sp)
    ret
return_probe:
    andi t0, sp, 15
    bnez t0, bad_stack
    la a0, one
    ret
bad_stack:
    li a0, -1
    ret
.globl vc_main
vc_main:
    addi sp, sp, -16
    sd ra, 8(sp)
    la a0, one
    li a1, 1
    la a2, args
    call vc_apply
    call vc_print_int
    call vc_print_newline
    la a0, three
    li a1, 3
    la a2, args
    call vc_apply
    call vc_print_int
    call vc_print_newline
    la a0, nine
    li a1, 9
    la a2, args
    call vc_apply
    call vc_print_int
    call vc_print_newline
    la a0, maker
    li a1, 2
    la a2, args
    call vc_apply
    call vc_print_int
    call vc_print_newline
    la a0, nine
    li a1, 2
    la a2, args
    call vc_apply
    li a1, 7
    la a2, args
    addi a2, a2, 16
    call vc_apply
    call vc_print_int
    call vc_print_newline
    ld ra, 8(sp)
    addi sp, sp, 16
    ret
