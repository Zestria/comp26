#!/bin/sh
# Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors
# SPDX-License-Identifier: LGPL-3.0-or-later
set -eu

if ! command -v pkg-config >/dev/null 2>&1 || ! pkg-config --exists libffi; then
    echo "runtime tests require pkg-config and native libffi development files" >&2
    exit 1
fi

# Function addresses are stored with an erased prototype; libffi supplies the
# actual signature. pkg-config flags intentionally undergo word splitting.
cc -std=c11 -Wall -Wextra -Werror -Wno-cast-function-type \
    $(pkg-config --cflags libffi) -I ../../backend \
    ../../backend/closure_runtime.c closure_runtime_tests.c \
    -o closure_runtime_native.exe $(pkg-config --libs libffi)
./closure_runtime_native.exe

if sh run_riscv.sh available; then
    sh run_riscv.sh runtime ../../backend
else
    echo "skipped RV64 runtime execution: cross-compiler, QEMU or target libffi not found"
fi
