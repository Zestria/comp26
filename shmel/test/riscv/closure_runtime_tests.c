/* Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#include "closure_runtime.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

typedef shmel_word word;

static shmel_closure *as_closure(word value) {
    return (shmel_closure *)(uintptr_t)value;
}

static word identity(word x) { return x; }
static word constant(void) { return UINT64_MAX; }
static word add(word x, word y) { return x + y; }
static word subtract(word x, word y) { return x - y; }

static word weighted8(word a, word b, word c, word d, word e, word f, word g, word h) {
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h;
}

static word weighted9(word a, word b, word c, word d, word e, word f, word g,
                      word h, word i) {
    return weighted8(a, b, c, d, e, f, g, h) + 9*i;
}

static word weighted10(word a, word b, word c, word d, word e, word f, word g,
                       word h, word i, word j) {
    return weighted9(a, b, c, d, e, f, g, h, i) + 10*j;
}

static shmel_closure *add_closure;
static shmel_closure *last_intermediate;

static word make_adder(word x) {
    word arg[] = {x};
    word result = shmel_apply(add_closure, 1, arg);
    last_intermediate = as_closure(result);
    return result;
}

static void test_partial_application(void) {
    add_closure = shmel_make_closure((shmel_code_address)add, 2);
    assert(add_closure->applied == 0 && add_closure->arity == 2);
    assert(shmel_apply(add_closure, 0, NULL) == (word)(uintptr_t)add_closure);
    word ten[] = {10}, twenty[] = {20}, seven[] = {7};
    shmel_closure *a = as_closure(shmel_apply(add_closure, 1, ten));
    shmel_closure *b = as_closure(shmel_apply(add_closure, 1, twenty));
    assert(a != b && a != add_closure);
    assert(add_closure->applied == 0);
    assert(a->code == add_closure->code && a->arity == 2);
    assert(a->applied == 1 && a->arguments[0] == 10);
    assert(shmel_apply(a, 1, seven) == 17);
    assert(shmel_apply(b, 1, seven) == 27);
    assert(shmel_apply(a, 1, seven) == 17);
    word pair[] = {9, 4};
    shmel_closure *sub = shmel_make_closure((shmel_code_address)subtract, 2);
    assert(shmel_apply(sub, 2, pair) == 5);
    shmel_closure *maker = shmel_make_closure((shmel_code_address)make_adder, 1);
    word extra[] = {10, 7};
    assert(shmel_apply(maker, 2, extra) == 17);
    free(last_intermediate);
    free(maker);
    free(sub);
    free(a);
    free(b);
    free(add_closure);
}

static void test_many_arguments(void) {
    word args[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    shmel_code_address codes[] = {
        (shmel_code_address)weighted8,
        (shmel_code_address)weighted9,
        (shmel_code_address)weighted10
    };
    for (uint64_t arity = 8; arity <= 10; ++arity) {
        shmel_closure *base = shmel_make_closure(codes[arity - 8], arity);
        word expected = 0;
        for (uint64_t i = 0; i < arity; ++i) {
            expected += args[i] * (i + 1);
        }
        assert(shmel_apply(base, arity, args) == expected);
        shmel_closure *p = as_closure(shmel_apply(base, 3, args));
        shmel_closure *q = as_closure(shmel_apply(p, 4, args + 3));
        assert(p->applied == 3 && q->applied == 7);
        assert(shmel_apply(q, arity - 7, args + 7) == expected);
        if (arity == 10) {
            shmel_closure *r = as_closure(shmel_apply(q, 2, args + 7));
            assert(r->applied == 9 && r->arguments[8] == 9);
            assert(shmel_apply(r, 1, args + 9) == expected);
            free(r);
        }
        free(q);
        free(p);
        free(base);
    }
}

static shmel_closure *fac_closure;
static shmel_closure *fac_step_closure;

static word fac_step(word n, word k, word result) {
    word args[] = {n * result};
    return shmel_apply(as_closure(k), 1, args);
}

static word fac_cps(word n, word k) {
    if (n <= 1) {
        word one[] = {1};
        return shmel_apply(as_closure(k), 1, one);
    }
    word saved[] = {n, k};
    word continuation = shmel_apply(fac_step_closure, 2, saved);
    word args[] = {n - 1, continuation};
    word result = shmel_apply(fac_closure, 2, args);
    free(as_closure(continuation));
    return result;
}

static shmel_closure *fib_closure;
static shmel_closure *fib_left_closure;
static shmel_closure *fib_right_closure;

static word fib_right(word left, word k, word right) {
    word args[] = {left + right};
    return shmel_apply(as_closure(k), 1, args);
}

static word fib_left(word n, word k, word left) {
    word saved[] = {left, k};
    word continuation = shmel_apply(fib_right_closure, 2, saved);
    word args[] = {n - 2, continuation};
    word result = shmel_apply(fib_closure, 2, args);
    free(as_closure(continuation));
    return result;
}

static word fib_cps(word n, word k) {
    if (n < 2) {
        word args[] = {n};
        return shmel_apply(as_closure(k), 1, args);
    }
    word saved[] = {n, k};
    word continuation = shmel_apply(fib_left_closure, 2, saved);
    word args[] = {n - 1, continuation};
    word result = shmel_apply(fib_closure, 2, args);
    free(as_closure(continuation));
    return result;
}

static void test_cps(void) {
    shmel_closure *id = shmel_make_closure((shmel_code_address)identity, 1);
    fac_closure = shmel_make_closure((shmel_code_address)fac_cps, 2);
    fac_step_closure = shmel_make_closure((shmel_code_address)fac_step, 3);
    fib_closure = shmel_make_closure((shmel_code_address)fib_cps, 2);
    fib_left_closure = shmel_make_closure((shmel_code_address)fib_left, 3);
    fib_right_closure = shmel_make_closure((shmel_code_address)fib_right, 3);
    word args[] = {10, (word)(uintptr_t)id};
    assert(shmel_apply(fac_closure, 2, args) == 3628800);
    assert(shmel_apply(fib_closure, 2, args) == 55);
    args[0] = 0;
    assert(shmel_apply(fac_closure, 2, args) == 1);
    assert(shmel_apply(fib_closure, 2, args) == 0);
    free(id);
    free(fac_closure);
    free(fac_step_closure);
    free(fib_closure);
    free(fib_left_closure);
    free(fib_right_closure);
}

int main(void) {
    assert(shmel_invoke(constant, 0, NULL) == UINT64_MAX);
    word all_bits[] = {UINT64_MAX};
    assert(shmel_invoke((shmel_code_address)identity, 1, all_bits) == UINT64_MAX);
    assert(all_bits[0] == UINT64_MAX);
    word negative[] = {4, 9};
    assert(shmel_invoke((shmel_code_address)subtract, 2, negative) == UINT64_MAX - 4);
    test_partial_application();
    test_many_arguments();
    test_cps();
    puts("closure runtime tests passed");
    return 0;
}
