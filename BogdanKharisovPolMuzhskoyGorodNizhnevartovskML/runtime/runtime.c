/* Copyright 2026, shrexer06
 * SPDX-License-Identifier: LGPL-3.0-or-later
 *
 * Runtime for the generated RISC-V 64 code. Must stay in sync with
 * backend/abi.mli.
 *
 * Values are 64-bit words (LP64 long). An immediate has its low bit set: the
 * integer n is 2n + 1, false and () are 1, true is 3. A heap value is the
 * 8-aligned address of its header word, so its low bit is clear.
 *
 * Header word: field_count << 1 for a tuple, 1 for a closure.
 * Tuple:   [header; v_0; ...; v_{n-1}]
 * Closure: [header = 1; code address; arity (raw); env_0; ...; env_{k-1}]
 * Partial application: a closure with code = rt_pap_code, arity = number of
 * missing arguments, environment [f; m (raw); a_1; ...; a_m].
 *
 * Code labels follow the RISC-V LP64 C convention for
 *   value code(value closure, value a_1, ..., value a_n):
 * a0 = the callee closure, a1..a7 = arguments 1..7, arguments 8.. at
 * 0(sp), 8(sp), ... in the caller's outgoing area at the call, result in a0.
 * s0-s11 and sp are preserved across the call, sp is 16-byte aligned.
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

typedef long value;

static void rt_fatal(const char *message)
{
    fflush(stdout);
    fputs(message, stderr);
    exit(2);
}

/* Fresh uninitialised block of [words] words (header included). No collector:
 * blocks live until the process exits. */
value rt_alloc(long words)
{
    value *block = malloc((size_t)words * sizeof(value));
    if (block == NULL)
        rt_fatal("Fatal error: out of memory\n");
    return (value)block;
}

/*
 * Call the code address [code] of the closure [clos] with the [n] arguments
 * argv[0..n-1], following the calling convention above.
 *
 * There is no bound on n other than the C stack: argument 8 and up are
 * spilled to the outgoing stack area, so a call needs 8 * max(n - 7, 0)
 * bytes of stack plus this frame, and n is limited by the size of the
 * stack (several hundred thousand arguments on the default 8 MiB main
 * stack). [rt_invoke] is written by hand, because C cannot express a call
 * with a runtime-determined number of arguments: it saves ra and s0,
 * reserves a 16-byte aligned frame, copies argv[7..n-1] to 0(sp), 8(sp),
 * ..., loads a1..a{k} from argv[0..min(n, 7) - 1] (only the arguments that
 * exist), sets a0 = clos, calls and restores sp, s0 and ra.
 *
 * C arguments: a0 = code, a1 = clos, a2 = argv, a3 = n.
 */
extern value rt_invoke(long code, value clos, value *argv, long n);
value rt_apply(value f, long n, value *argv);

__asm__(
    ".text\n"
    ".p2align 2\n"
    ".globl rt_invoke\n"
    ".type rt_invoke, @function\n"
    "rt_invoke:\n"
    "mv t0, a0\n"             /* code                          */
    "mv t1, a1\n"             /* clos                          */
    "mv t2, a2\n"             /* argv                          */
    "mv t3, a3\n"             /* n                             */
    "addi t5, t3, -7\n"
    "blez t5, .Lrti_no_stack\n"     /* n <= 7: no outgoing area */
    "slli t5, t5, 3\n"        /* (n - 7) * 8                   */
    "addi t5, t5, 15\n"
    "andi t4, t5, -16\n"      /* round up to a multiple of 16  */
    "j .Lrti_alloc\n"
    ".Lrti_no_stack:\n"
    "li t4, 0\n"
    ".Lrti_alloc:\n"
    "addi t4, t4, 16\n"       /* frame: 16 + outgoing area     */
    "sub sp, sp, t4\n"
    "add t5, sp, t4\n"        /* t5 = old sp                   */
    "sd ra, -8(t5)\n"
    "sd s0, -16(t5)\n"
    "mv s0, t5\n"             /* frame pointer = old sp        */
    /* Copy argv[7..n-1] to 0(sp), 8(sp), ... */
    "li t5, 7\n"
    "bge t5, t3, .Lrti_regs\n"
    "slli t5, t5, 3\n"        /* 56 = &argv[7] - argv          */
    "li t6, 0\n"              /* destination offset           */
    ".Lrti_copy:\n"
    "add a4, t2, t5\n"        /* &argv[t5 / 8]                 */
    "ld a5, 0(a4)\n"
    "add a6, sp, t6\n"
    "sd a5, 0(a6)\n"
    "addi t5, t5, 8\n"
    "addi t6, t6, 8\n"
    "slli a4, t3, 3\n"
    "blt t5, a4, .Lrti_copy\n"
    ".Lrti_regs:\n"
    /* Load a1..a{k} from argv[0..min(n, 7) - 1]; only existing ones. */
    "li t5, 7\n"
    "blt t3, t5, .Lrti_few\n"
    "ld a1, 0(t2)\n"
    "ld a2, 8(t2)\n"
    "ld a3, 16(t2)\n"
    "ld a4, 24(t2)\n"
    "ld a5, 32(t2)\n"
    "ld a6, 40(t2)\n"
    "ld a7, 48(t2)\n"
    "j .Lrti_call\n"
    ".Lrti_few:\n"
    "blez t3, .Lrti_call\n"
    "ld a1, 0(t2)\n"
    "li t5, 2\n"
    "blt t3, t5, .Lrti_call\n"
    "ld a2, 8(t2)\n"
    "li t5, 3\n"
    "blt t3, t5, .Lrti_call\n"
    "ld a3, 16(t2)\n"
    "li t5, 4\n"
    "blt t3, t5, .Lrti_call\n"
    "ld a4, 24(t2)\n"
    "li t5, 5\n"
    "blt t3, t5, .Lrti_call\n"
    "ld a5, 32(t2)\n"
    "li t5, 6\n"
    "blt t3, t5, .Lrti_call\n"
    "ld a6, 40(t2)\n"
    ".Lrti_call:\n"
    "mv a0, t1\n"             /* the callee closure            */
    "jalr t0\n"
    "mv t5, s0\n"             /* result in a0; restore         */
    "ld s0, -16(t5)\n"
    "ld ra, -8(t5)\n"
    "mv sp, t5\n"
    "ret\n"
    ".size rt_invoke, . - rt_invoke\n");

/* Code label of partial applications. [clos] is the PAP closure:
 * [1; rt_pap_code; rem; f; m; a_1; ...; a_m]; the [rem] missing arguments
 * arrive as arguments 2.. of this label, that is, as the variadic arguments
 * of this C function (on RISC-V LP64 variadic integer arguments are passed
 * exactly like named ones).
 *
 * The argument vector of m + rem values is a variable-length array, so it
 * lives on the C stack; it is bounded by the original function arity and
 * hence by the stack. */
value rt_pap_code(value clos, ...)
{
    long rem = ((long *)clos)[2];
    value f = ((value *)clos)[3];
    long m = ((long *)clos)[4];
    value argv[m + rem];
    long i;
    va_list ap;

    va_start(ap, clos);
    for (i = 0; i < m; i++)
        argv[i] = ((value *)clos)[5 + i];
    for (; i < m + rem; i++)
        argv[i] = va_arg(ap, value);
    va_end(ap);
    return rt_apply(f, m + rem, argv);
}

/* Generic application of the closure [f] to the [n] >= 1 arguments
 * argv[0..n-1]. [f] is applied to [arity] arguments at a time: exactly, or
 * partially (allocating a PAP), or repeatedly in case of over-application. */
value rt_apply(value f, long n, value *argv)
{
    for (;;) {
        value code = ((value *)f)[1];
        long arity = ((long *)f)[2];
        if (n == arity)
            return rt_invoke(code, f, argv, n);
        if (n < arity) {
            value *pap = (value *)rt_alloc(5 + n);
            long i;
            pap[0] = 1L;
            pap[1] = (value)rt_pap_code;
            pap[2] = arity - n;
            pap[3] = f;
            pap[4] = n;
            for (i = 0; i < n; i++)
                pap[5 + i] = argv[i];
            return (value)pap;
        }
        f = rt_invoke(code, f, argv, arity);
        argv += arity;
        n -= arity;
    }
}

static void rt_compare_functional(void)
{
    rt_fatal("Fatal error: exception Invalid_argument(\"compare: functional value\")\n");
}

/* Structural order of two values, raw -1, 0 or 1 (not tagged).
 * Immediates are compared as signed (the low tag bit preserves the order);
 * an immediate is ordered before a block (this cannot occur in a well-typed
 * program). Blocks: fields are compared left to right, the first difference
 * decides, sizes break ties. Reaching a closure, on either side, even the
 * same pointer, is a runtime error, so pointer equality is not
 * short-circuited. */
static long rt_compare_rec(value a, value b)
{
    value *pa;
    value *pb;
    long na;
    long nb;
    long i;

    if ((a & 1) != 0 && (b & 1) != 0) {
        if (a < b)
            return -1;
        if (a > b)
            return 1;
        return 0;
    }
    if ((a & 1) != 0 || (b & 1) != 0) {
        /* Mixed immediate and block: a closure on either side is a runtime
         * error even before the immediate-before-block ordering applies. */
        if (((value *)((a & 1) != 0 ? b : a))[0] == 1L)
            rt_compare_functional();
        return (a & 1) != 0 ? -1 : 1;
    }
    pa = (value *)a;
    pb = (value *)b;
    if (pa[0] == 1L || pb[0] == 1L)
        rt_compare_functional();
    na = pa[0] >> 1;
    nb = pb[0] >> 1;
    for (i = 0; i < na && i < nb; i++) {
        long c = rt_compare_rec(pa[1 + i], pb[1 + i]);
        if (c != 0)
            return c;
    }
    if (na < nb)
        return -1;
    if (na > nb)
        return 1;
    return 0;
}

long rt_compare(value a, value b)
{
    return rt_compare_rec(a, b);
}

value rt_print_int(value v)
{
    printf("%ld", v >> 1);
    return 1L;
}

void rt_div_by_zero(void)
{
    rt_fatal("Fatal error: exception Division_by_zero\n");
}

extern value ml_main(void);

int main(void)
{
    ml_main();
    fflush(stdout);
    return 0;
}