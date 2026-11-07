.text
.globl vc_main
vc_main:
    addi sp, sp, -16
    sd ra, 8(sp)
    li a0, 0
    call vc_print_int
    call vc_print_newline
    li a0, -5
    call vc_print_int
    call vc_print_newline
    # Границы 64-битного слова: int без тегов, см. ADR 0003.
    li a0, 9223372036854775807
    call vc_print_int
    call vc_print_newline
    li a0, -9223372036854775808
    call vc_print_int
    call vc_print_newline
    ld ra, 8(sp)
    addi sp, sp, 16
    ret
