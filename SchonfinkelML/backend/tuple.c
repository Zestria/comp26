/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "tuple.h"

#include "gc.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  bool is_reference;
  union {
    uintptr_t word;
    void *reference;
  } value;
} schoml_tuple_field;

struct schoml_tuple {
  size_t length;
  schoml_tuple_field fields[];
};

static _Noreturn void fail(const char *message) {
  fprintf(stderr, "SchonfinkelML tuple: %s\n", message);
  abort();
}

static void check_index(const schoml_tuple *tuple, size_t index) {
  if (index >= tuple->length) {
    fail("index out of bounds");
  }
}

static void trace_tuple(void *object, schoml_gc_visit_fn visit) {
  schoml_tuple *tuple = object;
  for (size_t i = 0; i < tuple->length; ++i) {
    if (tuple->fields[i].is_reference) {
      visit(tuple->fields[i].value.reference);
    }
  }
}

schoml_tuple *schoml_tuple_alloc(size_t length) {
  if (length > (SIZE_MAX - sizeof(schoml_tuple)) / sizeof(schoml_tuple_field)) {
    fail("allocation size overflow");
  }
  schoml_tuple *tuple = schoml_gc_alloc(
      sizeof(*tuple) + length * sizeof(schoml_tuple_field), trace_tuple);
  tuple->length = length;
  return tuple;
}

size_t schoml_tuple_length(const schoml_tuple *tuple) { return tuple->length; }

void schoml_tuple_set_word(schoml_tuple *tuple, size_t index, uintptr_t value) {
  check_index(tuple, index);
  tuple->fields[index].is_reference = false;
  tuple->fields[index].value.word = value;
}

uintptr_t schoml_tuple_get_word(const schoml_tuple *tuple, size_t index) {
  check_index(tuple, index);
  if (tuple->fields[index].is_reference) {
    fail("expected an immediate field");
  }
  return tuple->fields[index].value.word;
}

void schoml_tuple_set_ref(schoml_tuple *tuple, size_t index, void *value) {
  check_index(tuple, index);
  tuple->fields[index].is_reference = true;
  tuple->fields[index].value.reference = value;
}

void *schoml_tuple_get_ref(const schoml_tuple *tuple, size_t index) {
  check_index(tuple, index);
  if (!tuple->fields[index].is_reference) {
    fail("expected a reference field");
  }
  return tuple->fields[index].value.reference;
}
