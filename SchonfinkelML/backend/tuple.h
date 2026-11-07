/* SPDX-License-Identifier: LGPL-3.0-or-later */
#ifndef SCHOML_TUPLE_H
#define SCHOML_TUPLE_H

#include <stddef.h>
#include <stdint.h>

typedef struct schoml_tuple schoml_tuple;

/* Allocates a managed tuple. All fields initially contain the immediate 0.
 * The tuple must be rooted before the next allocation. */
schoml_tuple *schoml_tuple_alloc(size_t length);
size_t schoml_tuple_length(const schoml_tuple *tuple);

/* Immediate words and managed references are distinguished explicitly, without
 * stealing bits from integers or guessing whether a word looks like a pointer.
 * Bare code pointers must not be stored with set_ref. These accessors do not
 * allocate; an invalid index or a getter with the wrong field kind aborts. */
void schoml_tuple_set_word(schoml_tuple *tuple, size_t index, uintptr_t value);
uintptr_t schoml_tuple_get_word(const schoml_tuple *tuple, size_t index);
void schoml_tuple_set_ref(schoml_tuple *tuple, size_t index, void *value);
void *schoml_tuple_get_ref(const schoml_tuple *tuple, size_t index);

#endif
