/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "gc.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* The union makes (block + 1) suitable for any C object type. Metadata stays
 * outside the payload, so closure/tuple layouts do not depend on the GC. */
typedef union schoml_gc_block schoml_gc_block;
union schoml_gc_block {
  max_align_t alignment;
  struct {
    schoml_gc_block *next;
    schoml_gc_block *gray_next;
    schoml_gc_trace_fn trace;
    size_t bytes;
    bool marked;
  } info;
};

static schoml_gc_block *blocks;
static schoml_gc_block *gray;
static schoml_gc_root *roots;
static schoml_gc_stats stats;
static size_t heap_bytes;
static const size_t minimum_threshold = 4096;
static size_t threshold = 4096;
static bool collecting;

static _Noreturn void fail(const char *message) {
  fprintf(stderr, "SchonfinkelML GC: %s\n", message);
  abort();
}

static void check_not_collecting(void) {
  if (collecting) {
    fail("trace callbacks must only visit references");
  }
}

void schoml_gc_push_root(schoml_gc_root *root, void *value) {
  check_not_collecting();
  if (root == NULL) {
    fail("root record must not be NULL");
  }
  for (schoml_gc_root *cursor = roots; cursor != NULL;
       cursor = cursor->previous) {
    if (cursor == root) {
      fail("root is already registered");
    }
  }
  root->value = value;
  root->previous = roots;
  roots = root;
}

void schoml_gc_pop_root(schoml_gc_root *root) {
  check_not_collecting();
  if (root == NULL || root != roots) {
    fail("roots must be popped in reverse registration order");
  }
  roots = root->previous;
  root->value = NULL;
  root->previous = NULL;
}

static void mark(void *object) {
  if (object == NULL) {
    return;
  }
  schoml_gc_block *block = (schoml_gc_block *)object - 1;
  if (!block->info.marked) {
    block->info.marked = true;
    block->info.gray_next = gray;
    gray = block;
  }
}

void schoml_gc_collect(void) {
  check_not_collecting();
  collecting = true;
  for (schoml_gc_root *root = roots; root != NULL; root = root->previous) {
    mark(root->value);
  }
  /* An explicit worklist avoids overflowing the C stack on long object chains
   * and requires no additional allocations during collection. */
  while (gray != NULL) {
    schoml_gc_block *block = gray;
    gray = block->info.gray_next;
    block->info.gray_next = NULL;
    if (block->info.trace != NULL) {
      block->info.trace(block + 1, mark);
    }
  }
  schoml_gc_block **cursor = &blocks;
  while (*cursor != NULL) {
    schoml_gc_block *block = *cursor;
    if (block->info.marked) {
      block->info.marked = false;
      cursor = &block->info.next;
    } else {
      *cursor = block->info.next;
      stats.objects--;
      stats.bytes -= block->info.bytes;
      heap_bytes -= sizeof(*block) + block->info.bytes;
      free(block);
    }
  }
  stats.collections++;
  threshold = heap_bytes > SIZE_MAX / 2 ? SIZE_MAX : heap_bytes * 2;
  if (threshold < minimum_threshold) {
    threshold = minimum_threshold;
  }
  collecting = false;
}

void *schoml_gc_alloc(size_t bytes, schoml_gc_trace_fn trace) {
  check_not_collecting();
  if (bytes > SIZE_MAX - sizeof(schoml_gc_block)) {
    fail("allocation size overflow");
  }
  size_t total = sizeof(schoml_gc_block) + bytes;
  if (heap_bytes >= threshold || total > threshold - heap_bytes) {
    schoml_gc_collect();
  }
  if (total > SIZE_MAX - heap_bytes) {
    fail("heap size overflow");
  }
  schoml_gc_block *block = calloc(1, total);
  if (block == NULL) {
    schoml_gc_collect();
    block = calloc(1, total);
    if (block == NULL) {
      fail("out of memory");
    }
  }
  block->info.bytes = bytes;
  block->info.trace = trace;
  block->info.next = blocks;
  blocks = block;
  heap_bytes += total;
  stats.objects++;
  stats.bytes += bytes;
  return block + 1;
}

schoml_gc_stats schoml_gc_get_stats(void) { return stats; }

void schoml_gc_set_threshold(size_t bytes) {
  check_not_collecting();
  threshold = bytes;
}

void schoml_gc_shutdown(void) {
  check_not_collecting();
  if (roots != NULL) {
    fail("pop all roots before shutting down");
  }
  schoml_gc_collect();
  stats = (schoml_gc_stats){0};
  threshold = minimum_threshold;
}
