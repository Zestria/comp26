.globl _start

/* Functions called from generated code */
.globl vc_fail

.globl vc_apply
.globl vc_closure

.globl vc_print_int
.globl vc_print_newline

/* Generated main function */
.extern vc_main

/* IO streams */
.equ STDOUT, 1
.equ STDERR, 2

/* Number of syscalls */
.equ SYSCALL_WRITE, 64
.equ SYSCALL_EXIT_GROUP, 94
.equ SYSCALL_SIGACTION, 134
.equ SYSCALL_MMAP, 222

/* Constants for `mmap` syscall */
.equ PROT_READ_WRITE, 0x1 | 0x2
.equ MAP_PRIVATE_ANON, 0x02 | 0x20

.equ SIGSEGV, 11

.equ HEAP_SIZE, 1048576

.equ ERROR_ZERO_DIVISION, 0
.equ ERROR_MEMORY, 1

.section .rodata

error1_text: .ascii "Error: division by zero"
error2_text: .ascii "Error: not enough memory on heap"

errors:
    /* {length = 23; message = "Error: division by zero"} */
    .quad 23
    .quad error1_text

    /* {length = 32; message = "Error: not enough memory on heap"} */
    .quad 32
    .quad error2_text

sigaction_struct:
    .quad segfault_dispatch
    .quad 0
    .quad 0

.section .bss
.balign 8
.globl vc_heap_end
vc_heap_end:
    .zero 8

.section .text

/* void exit(long code) */
exit:
    li a7, SYSCALL_EXIT_GROUP
    ecall

/* void heap_init() */
heap_init:
    li a0, 0
    li a1, HEAP_SIZE
    li a2, PROT_READ_WRITE
    li a3, MAP_PRIVATE_ANON
    li a4, -1
    li a5, 0
    li a7, SYSCALL_MMAP
    ecall

    li t0, -4095
    bgeu a0, t0, .heap_failure
    mv s10, a0
    li t0, HEAP_SIZE
    add t0, s10, t0
    la t1, vc_heap_end
    sd t0, 0(t1)

    li    a0, SIGSEGV
    la    a1, sigaction_struct    /* pointer to the sigaction struct */
    li    a2, 0                   /* oldact = NULL */
    li    a3, 8                   /* sigsetsize = 8 */
    li    a7, SYSCALL_SIGACTION
    ecall

    ret

segfault_dispatch:
    li a0, ERROR_MEMORY
    tail vc_fail

/* void heap_alloc(size_t size)            */
heap_alloc:
    /* Проверяем конец блока, а не только первый байт: соседняя страница
       может быть отображена, поэтому одного SIGSEGV недостаточно. */
    add a1, s10, a0
    bltu a1, s10, .heap_failure
    la a2, vc_heap_end
    ld a2, 0(a2)
    bgtu a1, a2, .heap_failure
    mv a0, s10
    mv s10, a1

    ret

.heap_failure:
    li a0, ERROR_MEMORY
    tail vc_fail

/* void* vc_closure(void* code, long arity) */
vc_closure:
    addi sp, sp, -16
    sd ra, 8(sp)

    mv t1, a0
    mv t2, a1
    li a0, 24

    call heap_alloc

    sd t1, 0(a0)
    sd t2, 8(a0)
    sd zero, 16(a0)

    ld ra, 8(sp)
    addi sp, sp, 16
    ret

/* void* vc_apply(void* closure, long argc, long* argv) */
vc_apply:
    addi sp, sp, -48
    sd ra, 40(sp)
    sd s0, 32(sp)
    sd s1, 24(sp)
    sd s2, 16(sp)
    sd s3, 8(sp)

    ld t0, 0(a0)     /* closure's code */
    ld t1, 8(a0)     /* closure's arity */
    ld t2, 16(a0)    /* closure's arguments number */
    addi t3, a0, 24  /* closure's arguments pointer */

    mv t4, a1        /* argc */
    mv t5, a2        /* argv */

    add t6, t2, t4   /* total arguments */

    bge t6, t1, .call_start

/* Not enough arguments to call, creating another closure */
.new_closure_begin:
    slli a0, t6, 3
    addi a0, a0, 24

    call heap_alloc

    sd t0, 0(a0)
    sd t1, 8(a0)
    sd t6, 16(a0)

    addi a1, a0, 24

.new_closure_args_a_begin:
    beq t2, zero, .new_closure_args_b_begin

.new_closure_args_a_loop:
    ld a2, 0(t3)
    sd a2, 0(a1)
    addi a1, a1, 8
    addi t3, t3, 8
    addi t2, t2, -1
    bne t2, zero, .new_closure_args_a_loop

.new_closure_args_b_begin:
    beq t4, zero, .epilogue

.new_closure_args_b_loop:
    ld a2, 0(t5)
    sd a2, 0(a1)
    addi a1, a1, 8
    addi t5, t5, 8
    addi t4, t4, -1
    bne t4, zero, .new_closure_args_b_loop

.new_closure_finish:
    j .epilogue

/* Enough arguments to call */
.call_start:
    /* Массив всех аргументов отделён от исходящих стековых аргументов.
       sp не сдвигается при загрузке a0..a7 и всегда выровнен на 16. */
    mv s2, sp
    sub s1, t6, t1           /* лишние аргументы после первого вызова */
    slli a0, t6, 3
    addi a0, a0, 15
    andi a0, a0, -16
    li a1, 0
    li a2, 8
    ble t1, a2, .outgoing_size_ready
    addi a1, t1, -8
    slli a1, a1, 3
    addi a1, a1, 15
    andi a1, a1, -16
.outgoing_size_ready:
    sub sp, sp, a0
    sub sp, sp, a1
    add s3, sp, a1           /* объединённый argv */
    slli a0, t1, 3
    add s0, s3, a0          /* аргументы для результата-замыкания */
    mv t6, s3

/* Moving all arguments on stack */
.call_args_on_stack_a_begin:
    beq t2, zero, .call_args_on_stack_b_begin
    li a0, 0

.call_args_on_stack_a_loop:
    ld a1, 0(t3)
    sd a1, 0(t6)
    addi t3, t3, 8
    addi t6, t6, 8
    addi a0, a0, 1
    bne a0, t2, .call_args_on_stack_a_loop

.call_args_on_stack_b_begin:
    beq t4, zero, .call_args_on_registers
    li a0, 0

.call_args_on_stack_b_loop:
    ld a1, 0(t5)
    sd a1, 0(t6)
    addi t5, t5, 8
    addi t6, t6, 8
    addi a0, a0, 1
    bne a0, t4, .call_args_on_stack_b_loop

/* Moving arguments from stack into registers */
.call_args_on_registers:
    /* Аргументы с девятого копируем в отдельную область у текущего sp. */
    li a3, 8
    ble t1, a3, .load_registers
    addi a3, t1, -8
    addi a1, s3, 64
    mv a2, sp
.copy_stack_arguments:
    ld a0, 0(a1)
    sd a0, 0(a2)
    addi a1, a1, 8
    addi a2, a2, 8
    addi a3, a3, -1
    bnez a3, .copy_stack_arguments
.load_registers:
    /* First argument -> A0 */
    beqz t1, .call
    ld a0, 0(s3)

    /* Second argument -> A1 */
    li t6, 1
    beq t1, t6, .call
    ld a1, 8(s3)

    /* Third argument -> A2 */
    li t6, 2
    beq t1, t6, .call
    ld a2, 16(s3)

    /* Fourth argument -> A3 */
    li t6, 3
    beq t1, t6, .call
    ld a3, 24(s3)

    /* Fifth argument -> A4 */
    li t6, 4
    beq t1, t6, .call
    ld a4, 32(s3)

    /* Sixth argument -> A5 */
    li t6, 5
    beq t1, t6, .call
    ld a5, 40(s3)

    /* Seventh argument -> A6 */
    li t6, 6
    beq t1, t6, .call
    ld a6, 48(s3)

    /* Eighth argument -> A7 */
    li t6, 7
    beq t1, t6, .call
    ld a7, 56(s3)

    /* Other argument (nineth and higher) will be on stack, as ABI requires */

.call:
    jalr ra, t0, 0   /* code pointer of closure  */
    beqz s1, .restore_stack

.remained_args:
    mv a1, s1
    mv a2, s0
    call vc_apply

.restore_stack:
    mv sp, s2

.epilogue:
    ld s3, 8(sp)
    ld s2, 16(sp)
    ld s1, 24(sp)
    ld s0, 32(sp)
    ld ra, 40(sp)
    addi sp, sp, 48
    ret

/* void vc_print_int(long number) */
vc_print_int:
    addi sp, sp, -32
    addi t0, sp, 32

    li t1, 0   /* number of symbols */
    li t2, 0   /* sign flag */
    li t3, 10  /* base */

    bge a0, zero, .start
    li t2, 1
    neg a0, a0

.start:
    bne a0, zero, .loop

    addi t1, t1, 1
    addi t0, t0, -1
    li t5, '0'
    sb t5, 0(t0)
    j .write

.loop:
    remu t5, a0, t3
    divu a0, a0, t3
    addi t5, t5, '0'
    addi t0, t0, -1
    addi t1, t1, 1
    sb t5, 0(t0)
    bne a0, zero, .loop

.sign:
    beq t2, zero, .write
    addi t1, t1, 1
    addi t0, t0, -1
    li t5, '-'
    sb t5, 0(t0)

.write:
    li a0, STDOUT
    mv a1, t0
    mv a2, t1
    li a7, SYSCALL_WRITE

    ecall

    /* Результат функции ML — unit, а не число байтов от write. */
    li a0, 0
    addi sp, sp, 32
    ret

/* void vc_print_newline() */
vc_print_newline:
    addi sp, sp, -16
    li t0, 10
    sb t0, 0(sp)

    li a0, STDOUT
    mv a1, sp
    li a2, 1
    li a7, SYSCALL_WRITE
    ecall

    addi sp, sp, 16
    ret

/* void vc_fail(long error) */
vc_fail:
    la t0, errors
    slli t1, a0, 4
    add t0, t0, t1

    li a0, STDERR
    ld a1, 8(t0)
    ld a2, 0(t0)
    li a7, SYSCALL_WRITE
    ecall

    addi sp, sp, -16
    li a1, 10
    sb a1, 0(sp)

    li a0, STDERR
    mv a1, sp
    li a2, 1
    li a7, SYSCALL_WRITE
    ecall

    addi sp, sp, 16

    li a0, 1
    tail exit

_start:
    /* GNU ld может заменить la на адрес относительно gp. Без crt0 этот
       регистр надо настроить самим, иначе статические замыкания недоступны. */
    .option push
    .option norelax
    la gp, __global_pointer$
    .option pop
    call heap_init
    call vc_main

    li a0, 0
    tail exit
