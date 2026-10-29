# gdb-multiarch a.out -x tests/.gdbinit, когда scripts/run.sh -g 1234 ждёт.
set architecture riscv:rv64
target remote :1234
layout regs
break _start
