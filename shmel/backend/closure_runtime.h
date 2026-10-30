/* Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#ifndef SHMEL_CLOSURE_RUNTIME_H
#define SHMEL_CLOSURE_RUNTIME_H

#include <stddef.h>
#include <stdint.h>

typedef uint64_t shmel_word;

/* Storage type for a code address, not a uniform callable prototype.
   Entries use the RV64 integer psABI and return one shmel_word. Each accepts
   arity word arguments: the first eight in a0-a7, the rest on the stack.
   Call through a matching function type or the libffi-backed shmel_invoke;
   do not call this zero-argument storage type directly. No implicit closure
   pointer is passed: saved arguments precede the new arguments. */
typedef shmel_word (*shmel_code_address)(void);

typedef struct shmel_closure {
    shmel_code_address code;
    uint64_t arity;
    uint64_t applied;
    shmel_word arguments[];
} shmel_closure;

_Static_assert(sizeof(shmel_word) == 8, "Shmel values must occupy 8 bytes");
_Static_assert(sizeof(shmel_code_address) == 8, "Shmel requires 64-bit code addresses");
_Static_assert(sizeof(void *) == 8, "Shmel requires 64-bit pointers");
_Static_assert(offsetof(shmel_closure, code) == 0, "Unexpected code offset");
_Static_assert(offsetof(shmel_closure, arity) == 8, "Unexpected arity offset");
_Static_assert(offsetof(shmel_closure, applied) == 16, "Unexpected applied offset");
_Static_assert(offsetof(shmel_closure, arguments) == 24, "Unexpected argument offset");
_Static_assert(sizeof(shmel_closure) == 24, "Unexpected closure header size");

/* Creates the initial closure with no saved arguments. Arity must be positive.
   Allocation or contract failures print a diagnostic and abort. */
shmel_closure *shmel_make_closure(shmel_code_address code, uint64_t arity);

/* Applies new arguments, creating a fresh closure when not yet saturated.
   argc == 0 returns the original closure as a word; argv may then be NULL.
   With nonzero argc, argv must point to that many valid words. Overapplication
   requires function-valued intermediate results (as guaranteed by typing).
   Returned closures are heap objects; without GC they live until process exit. */
shmel_word shmel_apply(const shmel_closure *closure, uint64_t argc,
                       const shmel_word *argv);

/* libffi uses the platform's default ABI (RV64 psABI on the target).
   All parameters and the result must be shmel_word, and arity must match argc.
   argv is not modified. argc must fit in libffi's unsigned int argument count. */
shmel_word shmel_invoke(shmel_code_address code, uint64_t argc,
                        const shmel_word *argv);

#endif
