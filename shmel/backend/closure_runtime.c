/* Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#include "closure_runtime.h"

#include <ffi.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static _Noreturn void runtime_error(const char *message) {
  fprintf(stderr, "Shmel runtime: %s\n", message);
  abort();
}

static size_t argument_bytes(uint64_t count) {
  if (count > (SIZE_MAX - sizeof(shmel_closure)) / sizeof(shmel_word)) {
    runtime_error("closure argument count is too large");
  }
  return (size_t)count * sizeof(shmel_word);
}

static void *allocate(size_t bytes) {
  void *memory = malloc(bytes);
  if (memory == NULL) {
    runtime_error("out of memory");
  }
  return memory;
}

static void validate_closure(const shmel_closure *closure) {
  if (closure == NULL || closure->code == NULL || closure->arity == 0 ||
      closure->applied >= closure->arity) {
    runtime_error("invalid closure");
  }
  (void)argument_bytes(closure->arity);
}

shmel_word shmel_invoke(shmel_code_address code, uint64_t argc,
                        const shmel_word *argv) {
  if (code == NULL || (argc != 0 && argv == NULL) || argc > UINT_MAX) {
    runtime_error("invalid libffi invocation");
  }
  size_t bytes = argument_bytes(argc);
  ffi_type **types = NULL;
  void **values = NULL;
  shmel_word *arguments = NULL;
  if (argc != 0) {
    types = allocate((size_t)argc * sizeof(*types));
    values = allocate((size_t)argc * sizeof(*values));
    arguments = allocate(bytes);
    memcpy(arguments, argv, bytes);
    for (uint64_t i = 0; i < argc; ++i) {
      types[i] = &ffi_type_uint64;
      values[i] = &arguments[i];
    }
  }

  ffi_cif cif;
  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, (unsigned int)argc, &ffi_type_uint64,
                   types) != FFI_OK) {
    runtime_error("libffi could not prepare the call");
  }
  shmel_word result;
  ffi_call(&cif, FFI_FN(code), &result, values);
  free(arguments);
  free(values);
  free(types);
  return result;
}

shmel_closure *shmel_make_closure(shmel_code_address code, uint64_t arity) {
  if (code == NULL || arity == 0) {
    runtime_error("a function needs a code address and a positive arity");
  }
  (void)argument_bytes(arity);
  shmel_closure *closure = allocate(sizeof(*closure));
  closure->code = code;
  closure->arity = arity;
  closure->applied = 0;
  return closure;
}

shmel_word shmel_apply(const shmel_closure *closure, uint64_t argc,
                       const shmel_word *argv) {
  if (argc != 0 && argv == NULL) {
    runtime_error("nonempty application needs an argument array");
  }
  (void)argument_bytes(argc);

  for (;;) {
    validate_closure(closure);
    if (argc == 0) {
      return (shmel_word)(uintptr_t)closure;
    }

    uint64_t needed = closure->arity - closure->applied;
    size_t saved_bytes = argument_bytes(closure->applied);
    if (argc < needed) {
      uint64_t applied = closure->applied + argc;
      shmel_closure *partial =
          allocate(sizeof(*partial) + argument_bytes(applied));
      partial->code = closure->code;
      partial->arity = closure->arity;
      partial->applied = applied;
      if (saved_bytes != 0) {
        memcpy(partial->arguments, closure->arguments, saved_bytes);
      }
      memcpy(partial->arguments + closure->applied, argv, argument_bytes(argc));
      return (shmel_word)(uintptr_t)partial;
    }

    /* The invocation borrows argv when there are no saved arguments.
       Otherwise its temporary array must survive nested applications. */
    shmel_word *combined = NULL;
    const shmel_word *arguments = argv;
    if (saved_bytes != 0) {
      combined = allocate(argument_bytes(closure->arity));
      memcpy(combined, closure->arguments, saved_bytes);
      memcpy(combined + closure->applied, argv, argument_bytes(needed));
      arguments = combined;
    }
    shmel_word result = shmel_invoke(closure->code, closure->arity, arguments);
    free(combined);

    argc -= needed;
    if (argc == 0) {
      return result;
    }
    argv += needed;
    /* Values are untagged: the type checker must ensure that a function
       was returned before more arguments are applied. */
    closure = (const shmel_closure *)(uintptr_t)result;
  }
}
