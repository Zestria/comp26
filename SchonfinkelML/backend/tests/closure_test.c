/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "closure.h"
#include "gc.h"
#include "tuple.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>

/* Managed capture must survive GC. */
static void test_managed_capture(void) {
    schoml_gc_root root;
    schoml_tuple *t = schoml_tuple_alloc(1);
    schoml_tuple_set_word(t, 0, 42);
    schoml_gc_push_root(&root, t);

    schoml_closure_slot slot = { .is_ref = 1, .value = t };
    void *clos = schoml_closure_make((void *)0xdead, 1, 1, &slot);

    schoml_gc_collect();

    schoml_closure *c = schoml_untag(clos);
    assert(c->env[0].is_ref);
    assert(schoml_tuple_get_word(c->env[0].value, 0) == 42);

    schoml_gc_pop_root();
    schoml_gc_collect();
}

/* Immediate capture must not be traced (otherwise crash). */
static void test_immediate_capture(void) {
    schoml_closure_slot slot = { .is_ref = 0, .value = (void *)12345 };
    void *clos = schoml_closure_make((void *)0xbeef, 2, 1, &slot);
    schoml_gc_collect();
    schoml_closure *c = schoml_untag(clos);
    assert(!c->env[0].is_ref);
    assert((uintptr_t)c->env[0].value == 12345);
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

int main(void) {
    test_managed_capture();
    test_immediate_capture();
    test_unreachable_freed();
    printf("closure_test: OK\n");
    return 0;
}
