#!/bin/sh
# Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors
# SPDX-License-Identifier: LGPL-3.0-or-later
set -eu

# The target dispatcher is RV64 assembly. Other hosts use a test-only C
# adapter; the full target implementation is checked under QEMU.
case "$(uname -m)" in
    riscv64) invoke=../../backend/closure_invoke.S ;;
    *) invoke=closure_invoke_native.c ;;
esac
cc -std=c11 -Wall -Wextra -Werror -Wno-cast-function-type \
    -I ../../backend ../../backend/closure_runtime.c "$invoke" \
    closure_runtime_tests.c -o closure_runtime_native.exe
./closure_runtime_native.exe

if command -v riscv64-linux-gnu-gcc >/dev/null 2>&1 && \
   command -v qemu-riscv64 >/dev/null 2>&1; then
    sh run_riscv.sh runtime ../../backend
else
    echo "skipped RV64 runtime execution: cross-compiler or QEMU not found"
fi
