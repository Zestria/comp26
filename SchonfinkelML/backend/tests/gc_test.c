/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "gc.h"

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

typedef struct node {
  struct node *next;
  intptr_t number;
} node;

static void trace_node(void *object, schoml_gc_visit_fn visit) {
  node *value = object;
  visit(value->next);
}

static void test_roots_and_reassignment(void) {
  schoml_gc_root root;
  node *first = schoml_gc_alloc(sizeof(*first), trace_node);
  CHECK((uintptr_t)first % _Alignof(max_align_t) == 0);
  CHECK(first->next == NULL && first->number == 0);
  first->number = 42;
  schoml_gc_push_root(&root, first);
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 1);
  CHECK(first->number == 42);

  node *second = schoml_gc_alloc(sizeof(*second), trace_node);
  second->number = 99;
  root.value = second;
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 1);
  CHECK(((node *)root.value)->number == 99);
  root.value = NULL;
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 0);
  schoml_gc_pop_root();
  schoml_gc_shutdown();
}

static void test_cycles_and_shared_children(void) {
  schoml_gc_root left_root, right_root;
  node *left = schoml_gc_alloc(sizeof(*left), trace_node);
  schoml_gc_push_root(&left_root, left);
  node *right = schoml_gc_alloc(sizeof(*right), trace_node);
  schoml_gc_push_root(&right_root, right);
  node *shared = schoml_gc_alloc(sizeof(*shared), trace_node);
  left->next = shared;
  right->next = shared;
  shared->next = left;
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 3);
  schoml_gc_pop_root();
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 2);
  CHECK(left->next->next == left);
  schoml_gc_pop_root();
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 0);
  CHECK(schoml_gc_get_stats().bytes == 0);
  schoml_gc_shutdown();
}

static void test_automatic_collection(void) {
  schoml_gc_root root;
  node *live = schoml_gc_alloc(sizeof(*live), trace_node);
  live->number = 123;
  schoml_gc_push_root(&root, live);
  for (size_t i = 0; i < 2000; ++i) {
    (void)schoml_gc_alloc(128, NULL);
  }
  CHECK(schoml_gc_get_stats().collections > 0);
  CHECK(live->number == 123);
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 1);
  CHECK(schoml_gc_get_stats().bytes == sizeof(node));
  schoml_gc_pop_root();
  schoml_gc_shutdown();
}

static void test_deep_graph(void) {
  schoml_gc_root root;
  schoml_gc_push_root(&root, NULL);
  for (size_t i = 0; i < 100000; ++i) {
    node *next = schoml_gc_alloc(sizeof(*next), trace_node);
    next->next = root.value;
    root.value = next;
  }
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 100000);
  schoml_gc_pop_root();
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 0);
  schoml_gc_shutdown();
}

static void test_zero_size_and_empty_heap(void) {
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 0);
  schoml_gc_set_threshold(0);
  void *object = schoml_gc_alloc(0, NULL);
  CHECK(object != NULL);
  schoml_gc_root root;
  schoml_gc_push_root(&root, object);
  schoml_gc_collect();
  CHECK(schoml_gc_get_stats().objects == 1);
  CHECK(schoml_gc_get_stats().bytes == 0);
  schoml_gc_pop_root();
  schoml_gc_shutdown();
  CHECK(schoml_gc_get_stats().objects == 0);
  CHECK(schoml_gc_get_stats().collections == 0);
}

int main(void) {
  test_roots_and_reassignment();
  test_cycles_and_shared_children();
  test_automatic_collection();
  test_deep_graph();
  test_zero_size_and_empty_heap();
  puts("GC tests passed");
  return 0;
}
