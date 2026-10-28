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

.equ HEAP_SIZE, 4096

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

    mv s10, a0

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
    /* Function only modify a0 & a1 registers  */

    /* Ensure that new heap pointer is valid memory */
    /* If not - segmentation fault will occur, sigaction catch it and error message will be print */
    lb zero, 0(s10)

    mv a1, a0
    mv a0, s10
    add s10, a0, a1

    ret

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
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    sd s1, 8(sp)
    sd s2, 0(sp)

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
    /* Allocate space for arguments on stack */
    mv s2, sp

    /* Stack alignment */
    andi a0, t6, 1
    slli a0, a0, 3
    sub sp, sp, a0
    mv s1, sp

    sub t6, t6, t1
    slli t6, t6, 3
    slli t1, t1, 3

    sub sp, sp, t6   /* sp = sp - (total_args - arity) */
    mv s0, sp

    sub sp, sp, t1   /* sp = sp - (arity) */
    mv t6, sp

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
    /* First argument -> A0 */
    beq s0, sp, .call
    ld a0, 0(sp)
    addi sp, sp, 8

    /* Second argument -> A1 */
    beq s0, sp, .call
    ld a1, 0(sp)
    addi sp, sp, 8

    /* Third argument -> A2 */
    beq s0, sp, .call
    ld a2, 0(sp)
    addi sp, sp, 8

    /* Fourth argument -> A3 */
    beq s0, sp, .call
    ld a3, 0(sp)
    addi sp, sp, 8

    /* Fifth argument -> A4 */
    beq s0, sp, .call
    ld a4, 0(sp)
    addi sp, sp, 8

    /* Sixth argument -> A5 */
    beq s0, sp, .call
    ld a5, 0(sp)
    addi sp, sp, 8

    /* Seventh argument -> A6 */
    beq s0, sp, .call
    ld a6, 0(sp)
    addi sp, sp, 8

    /* Eighth argument -> A7 */
    beq s0, sp, .call
    ld a7, 0(sp)
    addi sp, sp, 8

    /* Other argument (nineth and higher) will be on stack, as ABI requires */

.call:
    jalr ra, t0, 0   /* code pointer of closure  */
    beq s0, s1, .restore_stack

.remained_args:
    sub a1, s1, s0
    srai a1, a1, 3

    mv sp, s0
    mv a2, s0

    /* Stack alignment */
    andi a3, a2, 1
    slli a3, a3, 3
    sub sp, sp, a3

    call vc_apply

.restore_stack:
    mv sp, s2

.epilogue:
    ld s2, 0(sp)
    ld s1, 8(sp)
    ld s0, 16(sp)
    ld ra, 24(sp)
    addi sp, sp, 32
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
    call heap_init
    call vc_main

    li a0, 0
    tail exit
