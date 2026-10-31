#!/bin/sh
# Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors
# SPDX-License-Identifier: LGPL-3.0-or-later
set -eu

# Non-RISC-V hosts use a test-only C adapter. QEMU runs the actual dispatcher.
case "$(uname -m)" in
    riscv64) invoke=../../backend/closure_invoke.S ;;
    *) invoke=closure_invoke_native.c ;;
esac
cc -std=c11 -Wall -Wextra -Werror -Wno-cast-function-type \
    -I ../../backend ../../backend/closure_runtime.c \
    "$invoke" closure_runtime_tests.c -o closure_runtime_native.exe
./closure_runtime_native.exe

if sh run_riscv.sh available; then
    sh run_riscv.sh runtime ../../backend
else
    echo "skipped RV64 runtime execution: cross-compiler or QEMU not found"
fi
