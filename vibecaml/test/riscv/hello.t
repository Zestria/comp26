Программа на ассемблере после peephole: пустой mv убран.

  $ ./hello.exe
      .text
      .globl vc_main
  vc_main:
      addi sp, sp, -16
      sd ra, 8(sp)  # call затрёт ra
      li a0, 42
      call vc_print_int
      call vc_print_newline
      ld ra, 8(sp)
      addi sp, sp, 16
      ret
