/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "closure.h"
#include "gc.h"
#include "tuple.h"

#include <assert.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <unistd.h>

/* Managed capture must survive GC. */
static void test_managed_capture(void) {
    schoml_gc_root root, root_clos;
    schoml_tuple *t = schoml_tuple_alloc(1);
    schoml_tuple_set_word(t, 0, 42);
    schoml_gc_push_root(&root, t);

    schoml_closure_slot slot = { .is_ref = 1, .value = t };
    void *clos = schoml_closure_make((void *)0xdead, 1, 1, &slot);
    schoml_gc_push_root(&root_clos, schoml_untag(clos));

    schoml_gc_collect();

    schoml_closure *c = schoml_untag(clos);
    assert(c->env[0].is_ref);
    assert(schoml_tuple_get_word(c->env[0].value, 0) == 42);

    schoml_gc_pop_root();
    schoml_gc_pop_root();
    schoml_gc_collect();
}

/* Immediate capture must not be traced (otherwise crash). */
static void test_immediate_capture(void) {
    schoml_gc_root root;
    schoml_closure_slot slot = { .is_ref = 0, .value = (void *)12345 };
    void *clos = schoml_closure_make((void *)0xbeef, 2, 1, &slot);
    schoml_gc_push_root(&root, schoml_untag(clos));
    schoml_gc_collect();
    schoml_closure *c = schoml_untag(clos);
    assert(!c->env[0].is_ref);
    assert((uintptr_t)c->env[0].value == 12345);
    schoml_gc_pop_root();
    schoml_gc_collect();
}

/* Unreachable closure and its captures are freed. */
static void test_unreachable_freed(void) {
    size_t before = schoml_gc_get_stats().objects;
    {
        schoml_gc_root r;
        schoml_tuple *t = schoml_tuple_alloc(1);
        schoml_gc_push_root(&r, t);
        schoml_closure_slot slot = { .is_ref = 1, .value = t };
        (void)schoml_closure_make((void *)0x1, 1, 1, &slot);
        schoml_gc_pop_root();
    }
    schoml_gc_collect();
    assert(schoml_gc_get_stats().objects == before);
}

/* Object and function pointers have equal size on POSIX; bridge without a
 * non-portable cast (the test builds with -pedantic -Werror). */
static void *bridge1(void *(*f)(void *)) {
    void *p;
    memcpy(&p, &f, sizeof p);
    return p;
}
static void *bridge2(void *(*f)(void *, void *)) {
    void *p;
    memcpy(&p, &f, sizeof p);
    return p;
}
static void *bridge3(void *(*f)(void *, void *, void *)) {
    void *p;
    memcpy(&p, &f, sizeof p);
    return p;
}
static void *bridge4(void *(*f)(void *, void *, void *, void *)) {
    void *p;
    memcpy(&p, &f, sizeof p);
    return p;
}

/* The closure tag is bit 0 of the pointer. On RISC-V (the codegen target)
 * code addresses are at least 2-byte aligned, so raw function pointers never
 * look like closures. x86-64 allows odd function addresses, so align the
 * test functions to keep the tag check meaningful on the host. */
#if defined(__GNUC__)
#define RAW_CODE __attribute__((aligned(2)))
#else
#define RAW_CODE
#endif

static RAW_CODE void *code_add1(void *a) { return (void *)((uintptr_t)a + 1); }
static RAW_CODE void *code_sum2(void *a, void *b) {
    return (void *)((uintptr_t)a + (uintptr_t)b);
}
static RAW_CODE void *code_sum3(void *a, void *b, void *c) {
    return (void *)((uintptr_t)a + (uintptr_t)b + (uintptr_t)c);
}
static RAW_CODE void *code_sum4(void *a, void *b, void *c, void *d) {
    return (void *)((uintptr_t)a + (uintptr_t)b + (uintptr_t)c + (uintptr_t)d);
}
/* Over-application support: ignore the argument, return a raw function. */
static RAW_CODE void *code_ret_add1(void *ignored) {
    (void)ignored;
    return bridge1(code_add1);
}

static void test_raw_apply(void) {
    assert((uintptr_t)schoml_closure_apply1(bridge1(code_add1), (void *)41) ==
           42);
    assert((uintptr_t)schoml_closure_apply2(bridge2(code_sum2), (void *)10,
                                            (void *)32) == 42);
}

static void test_full_application(void) {
    /* arity 1, applied 0 */
    void *c1 = schoml_closure_make(bridge1(code_add1), 1, 0, NULL);
    assert((uintptr_t)schoml_closure_apply1(c1, (void *)41) == 42);

    /* arity 2, applied 0 */
    void *c2 = schoml_closure_make(bridge2(code_sum2), 2, 0, NULL);
    assert((uintptr_t)schoml_closure_apply2(c2, (void *)10, (void *)32) == 42);

    /* arity 3, applied 1 */
    schoml_closure_slot caps1[1] = { { .is_ref = 0, .value = (void *)1 } };
    void *c3 = schoml_closure_make(bridge3(code_sum3), 3, 1, caps1);
    assert((uintptr_t)schoml_closure_apply2(c3, (void *)10, (void *)31) == 42);

    /* arity 4, applied 2 (CPS-fib continuation shape) */
    schoml_closure_slot caps2[2] = { { .is_ref = 0, .value = (void *)1 },
                                     { .is_ref = 0, .value = (void *)2 } };
    void *c4 = schoml_closure_make(bridge4(code_sum4), 4, 2, caps2);
    assert((uintptr_t)schoml_closure_apply2(c4, (void *)10, (void *)29) == 42);

    /* arity 4, applied 3 */
    schoml_closure_slot caps3[3] = { { .is_ref = 0, .value = (void *)1 },
                                     { .is_ref = 0, .value = (void *)2 },
                                     { .is_ref = 0, .value = (void *)3 } };
    void *c5 = schoml_closure_make(bridge4(code_sum4), 4, 3, caps3);
    assert((uintptr_t)schoml_closure_apply1(c5, (void *)36) == 42);
}

static void test_partial_application(void) {
    /* Chain of single arguments up to a full arity-3 application. */
    void *c = schoml_closure_make(bridge3(code_sum3), 3, 0, NULL);
    void *c1 = schoml_closure_apply1(c, (void *)1);
    void *c2 = schoml_closure_apply1(c1, (void *)2);
    schoml_closure *pc2 = schoml_untag(c2);
    assert(pc2->arity == 3);
    assert(pc2->applied == 2);
    assert((uintptr_t)schoml_closure_apply1(c2, (void *)39) == 42);

    /* Partial application on top of existing captures. */
    schoml_closure_slot caps[1] = { { .is_ref = 0, .value = (void *)10 } };
    void *d = schoml_closure_make(bridge3(code_sum3), 3, 1, caps);
    void *d1 = schoml_closure_apply1(d, (void *)20);
    assert((uintptr_t)schoml_closure_apply1(d1, (void *)12) == 42);
}

static void test_over_application(void) {
    /* arity 1 closure returning a raw function: the extra argument is
     * applied to the result. */
    void *c = schoml_closure_make(bridge1(code_ret_add1), 1, 0, NULL);
    assert((uintptr_t)schoml_closure_apply2(c, (void *)0, (void *)41) == 42);
}

/* A managed capture added by partial application keeps its own environment
 * alive across the collecting allocation inside apply. */
static void test_partial_application_gc(void) {
    schoml_gc_root rleaf, rmid, rparent, rresult;
    void *leaf = schoml_closure_make(bridge1(code_add1), 1, 0, NULL);
    schoml_gc_push_root(&rleaf, schoml_untag(leaf));

    schoml_closure_slot mid_caps[1] = { { .is_ref = 1, .value = leaf } };
    void *mid = schoml_closure_make(bridge1(code_add1), 1, 1, mid_caps);
    schoml_gc_push_root(&rmid, schoml_untag(mid));

    void *parent = schoml_closure_make(bridge2(code_sum2), 2, 0, NULL);
    schoml_gc_push_root(&rparent, schoml_untag(parent));

    /* Force a collection inside the allocation performed by apply. */
    schoml_gc_set_threshold(0);
    void *result = schoml_closure_apply1(parent, mid);

    /* Release the old roots (LIFO); the new closure keeps mid, and through
       it leaf, reachable. No collection happens while switching roots. */
    schoml_gc_pop_root();
    schoml_gc_pop_root();
    schoml_gc_pop_root();
    schoml_gc_push_root(&rresult, schoml_untag(result));
    schoml_gc_collect();

    schoml_closure *outer = schoml_untag(result);
    assert(outer->applied == 1);
    assert(schoml_is_closure(outer->env[0].value));
    schoml_closure *kept_mid = schoml_untag(outer->env[0].value);
    assert(kept_mid->applied == 1);
    assert(schoml_is_closure(kept_mid->env[0].value));
    schoml_closure *kept_leaf = schoml_untag(kept_mid->env[0].value);
    assert(kept_leaf->code == bridge1(code_add1));

    schoml_gc_pop_root();
}

static void expect_abort(void (*action)(void)) {
    pid_t child = fork();
    assert(child >= 0);
    if (child == 0) {
        close(STDERR_FILENO);
        action();
        _Exit(0);
    }
    int status;
    assert(waitpid(child, &status, 0) == child);
    assert(WIFSIGNALED(status) && WTERMSIG(status) == SIGABRT);
}

static void apply_arity_too_large(void) {
    void *c = schoml_closure_make(bridge1(code_add1), 9, 0, NULL);
    (void)schoml_closure_apply1(c, (void *)1);
}

static void apply_zero_arity(void) {
    void *c = schoml_closure_make(bridge1(code_add1), 0, 0, NULL);
    (void)schoml_closure_apply1(c, (void *)1);
}

int main(void) {
    struct rlimit no_core = {0, 0};
    assert(setrlimit(RLIMIT_CORE, &no_core) == 0);

    test_managed_capture();
    test_immediate_capture();
    test_unreachable_freed();
    test_raw_apply();
    test_full_application();
    test_partial_application();
    test_over_application();
    test_partial_application_gc();
    expect_abort(apply_arity_too_large);
    expect_abort(apply_zero_arity);

    schoml_gc_shutdown();
    printf("closure_runtime_test: passed\n");
    return 0;
}
