.text
.globl vc_main
vc_main:
    addi sp, sp, -16
    sd ra, 8(sp)
    li a0, 123
    call vc_print_int
    # Второй вызов печатает возвращённый unit: ожидаем 1230, а не 1233.
    call vc_print_int
    call vc_print_newline
    ld ra, 8(sp)
    addi sp, sp, 16
    ret
