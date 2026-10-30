#!/bin/sh
# Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors
# SPDX-License-Identifier: LGPL-3.0-or-later
set -eu

# The same C dispatcher runs on the host and on RV64 under QEMU.
cc -std=c11 -Wall -Wextra -Werror -Wno-cast-function-type \
    -I ../../backend ../../backend/closure_runtime.c \
    closure_runtime_tests.c -o closure_runtime_native.exe
./closure_runtime_native.exe

if command -v riscv64-linux-gnu-gcc >/dev/null 2>&1 && \
   command -v qemu-riscv64 >/dev/null 2>&1; then
    sh run_riscv.sh runtime ../../backend
else
    echo "skipped RV64 runtime execution: cross-compiler or QEMU not found"
fi
