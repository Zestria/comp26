/* Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

/* Test-only adapter for non-RISC-V hosts. Production uses closure_invoke.S
   without a fixed arity limit. This adapter covers the native runtime suite. */
#include "closure_runtime.h"

#include <stdio.h>
#include <stdlib.h>

typedef shmel_word word;

word shmel_invoke_abi(shmel_code_address code, uint64_t argc, const word *v) {
    switch (argc) {
    case 0:
        return code();
    case 1:
        return ((word (*)(word))code)(v[0]);
    case 2:
        return ((word (*)(word, word))code)(v[0], v[1]);
    case 3:
        return ((word (*)(word, word, word))code)(v[0], v[1], v[2]);
    case 4:
        return ((word (*)(word, word, word, word))code)(v[0], v[1], v[2], v[3]);
    case 5:
        return ((word (*)(word, word, word, word, word))code)(v[0], v[1], v[2], v[3], v[4]);
    case 6:
        return ((word (*)(word, word, word, word, word, word))code)(
            v[0], v[1], v[2], v[3], v[4], v[5]);
    case 7:
        return ((word (*)(word, word, word, word, word, word, word))code)(
            v[0], v[1], v[2], v[3], v[4], v[5], v[6]);
    case 8:
        return ((word (*)(word, word, word, word, word, word, word, word))code)(
            v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]);
    case 9:
        return ((word (*)(word, word, word, word, word, word, word, word, word))code)(
            v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8]);
    case 10:
        return ((word (*)(word, word, word, word, word, word, word, word, word, word))code)(
            v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
    default:
        fputs("test dispatcher: unsupported native-test arity\n", stderr);
        abort();
    }
}
