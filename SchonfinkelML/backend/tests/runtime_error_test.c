/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "runtime.h"

#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <unistd.h>

#define CHECK(condition)                                                       \
  do {                                                                         \
    if (!(condition)) {                                                        \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition);          \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
  } while (0)

static void allocation_overflow(void) { (void)schoml_gc_alloc(SIZE_MAX, NULL); }

static void tuple_size_overflow(void) { (void)schoml_tuple_alloc(SIZE_MAX); }

static void tuple_index_out_of_bounds(void) {
  schoml_tuple *tuple = schoml_tuple_alloc(0);
  schoml_tuple_set_word(tuple, 0, 1);
}

static void wrong_field_kind(void) {
  schoml_tuple *tuple = schoml_tuple_alloc(1);
  (void)schoml_tuple_get_ref(tuple, 0);
}

static void incorrect_root_order(void) {
  schoml_gc_root first, second;
  schoml_gc_push_root(&first, NULL);
  schoml_gc_push_root(&second, NULL);
  schoml_gc_pop_root(&first);
}

static void expect_abort(void (*action)(void), const char *diagnostic) {
  int output[2];
  CHECK(pipe(output) == 0);
  pid_t child = fork();
  CHECK(child >= 0);
  if (child == 0) {
    close(output[0]);
    if (dup2(output[1], STDERR_FILENO) < 0) {
      _Exit(2);
    }
    close(output[1]);
    action();
    _Exit(0);
  }
  close(output[1]);
  char message[512];
  size_t used = 0;
  ssize_t count;
  while ((count = read(output[0], message + used, sizeof(message) - 1 - used)) >
         0) {
    used += (size_t)count;
    CHECK(used < sizeof(message) - 1);
  }
  CHECK(count == 0);
  message[used] = '\0';
  close(output[0]);
  int status;
  CHECK(waitpid(child, &status, 0) == child);
  CHECK(WIFSIGNALED(status) && WTERMSIG(status) == SIGABRT);
  CHECK(strstr(message, diagnostic) != NULL);
}

int main(void) {
  /* Failure cases must not leave core dumps in the source/build directory. */
  struct rlimit no_core = {0, 0};
  CHECK(setrlimit(RLIMIT_CORE, &no_core) == 0);
  expect_abort(allocation_overflow, "GC: allocation size overflow");
  expect_abort(tuple_size_overflow, "tuple: allocation size overflow");
  expect_abort(tuple_index_out_of_bounds, "tuple: index out of bounds");
  expect_abort(wrong_field_kind, "tuple: expected a reference field");
  expect_abort(incorrect_root_order,
               "GC: roots must be popped in reverse registration order");
  puts("Runtime error tests passed");
  return 0;
}
