/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "gc.h"
#include "tuple.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define CHECK(condition)                                                       \
  do {                                                                         \
    if (!(condition)) {                                                        \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition);          \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
  } while (0)

static void test_tuple_fields(void) {
  schoml_gc_root root;
  schoml_tuple *tuple = schoml_tuple_alloc(3);
  schoml_gc_push_root(&root, tuple);
  CHECK(schoml_tuple_length(tuple) == 3);
  CHECK(schoml_tuple_get_word(tuple, 0) == 0);
  schoml_tuple_set_word(tuple, 0, UINTPTR_MAX);
  schoml_tuple_set_word(tuple, 1, (uintptr_t)INTPTR_MIN);
  schoml_tuple_set_ref(tuple, 2, NULL);
  schoml_gc_collect();
  CHECK(schoml_tuple_get_word(tuple, 0) == UINTPTR_MAX);
  CHECK(schoml_tuple_get_word(tuple, 1) == (uintptr_t)INTPTR_MIN);
  CHECK(schoml_tuple_get_ref(tuple, 2) == NULL);
  schoml_gc_pop_root(&root);
  schoml_gc_shutdown();
}

static void test_nested_tuples_and_cycles(void) {
  schoml_gc_root root;
  schoml_tuple *outer = schoml_tuple_alloc(2);
  schoml_gc_push_root(&root, outer);
  schoml_gc_set_threshold(0);
  schoml_tuple *inner = schoml_tuple_alloc(1);
  schoml_tuple_set_ref(outer, 0, inner);
  schoml_tuple_set_ref(outer, 1, inner);
  schoml_tuple_set_ref(inner, 0, outer);
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 2);
  CHECK(schoml_tuple_get_ref(inner, 0) == outer);
  schoml_tuple_set_word(outer, 0, 42);
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 2);
  schoml_tuple_set_word(outer, 1, 43);
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 1);
  schoml_gc_pop_root(&root);
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 0);
  schoml_gc_shutdown();
}

static void test_integer_is_not_a_reference(void) {
  schoml_gc_root root;
  schoml_tuple *tuple = schoml_tuple_alloc(1);
  schoml_gc_push_root(&root, tuple);
  void *unreachable = schoml_gc_alloc(32, NULL);
  uintptr_t address_as_integer = (uintptr_t)unreachable;
  schoml_tuple_set_word(tuple, 0, address_as_integer);
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 1);
  CHECK(schoml_tuple_get_word(tuple, 0) == address_as_integer);
  schoml_gc_pop_root(&root);
  schoml_gc_shutdown();
}

/* This is a test-only closure layout. The real closure representation and
 * application machinery belong to the separate closure-runtime task. */
typedef struct {
  uintptr_t (*code)(schoml_tuple *environment, uintptr_t argument);
  schoml_tuple *environment;
} test_closure;

static void trace_closure(void *object, schoml_gc_visit_fn visit) {
  test_closure *closure = object;
  visit(
      closure->environment); /* The code pointer is deliberately not visited. */
}

static uintptr_t add_capture(schoml_tuple *environment, uintptr_t argument) {
  return schoml_tuple_get_word(environment, 0) + argument;
}

static void test_closure_lifetime(void) {
  schoml_gc_root root;
  schoml_tuple *environment = schoml_tuple_alloc(2);
  schoml_gc_push_root(&root, environment);
  schoml_tuple_set_word(environment, 0, 40);
  schoml_gc_set_threshold(0);
  test_closure *closure = schoml_gc_alloc(sizeof(*closure), trace_closure);
  closure->code = add_capture;
  closure->environment = environment;
  root.value = closure;
  /* The environment also refers to the closure: an unreachable recursive
   * closure/environment cycle must be reclaimable. */
  schoml_tuple_set_ref(environment, 1, closure);
  for (size_t i = 0; i < 1000; ++i) {
    (void)schoml_tuple_alloc(4);
  }
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 2);
  CHECK(closure->code(closure->environment, 2) == 42);
  CHECK(schoml_tuple_get_ref(closure->environment, 1) == closure);
  schoml_gc_pop_root(&root);
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 0);
  CHECK(schoml_gc_get_stats().bytes == 0);
  schoml_gc_shutdown();
}

static void test_empty_tuple(void) {
  schoml_gc_root root;
  schoml_tuple *tuple = schoml_tuple_alloc(0);
  schoml_gc_push_root(&root, tuple);
  schoml_gc_collect();
  CHECK(schoml_tuple_length(tuple) == 0);
  CHECK(schoml_gc_get_stats().objects == 1);
  schoml_gc_pop_root(&root);
  schoml_gc_shutdown();
}

int main(void) {
  test_tuple_fields();
  test_nested_tuples_and_cycles();
  test_integer_is_not_a_reference();
  test_closure_lifetime();
  test_empty_tuple();
  puts("Tuple and closure lifetime tests passed");
  return 0;
}
