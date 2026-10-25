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

if command -v riscv64-linux-gnu-gcc >/dev/null 2>&1 && \
   command -v qemu-riscv64 >/dev/null 2>&1; then
    cross_pc="${RISCV_PKG_CONFIG_LIBDIR:-/usr/lib/riscv64-linux-gnu/pkgconfig}"
    if ! PKG_CONFIG_PATH= PKG_CONFIG_LIBDIR="$cross_pc" pkg-config --exists libffi; then
        echo "RV64 tests require target libffi development files; set RISCV_PKG_CONFIG_LIBDIR" >&2
        exit 1
    fi
    ffi_cflags=$(PKG_CONFIG_PATH= PKG_CONFIG_LIBDIR="$cross_pc" pkg-config --cflags libffi)
    ffi_libs=$(PKG_CONFIG_PATH= PKG_CONFIG_LIBDIR="$cross_pc" pkg-config --libs --static libffi)
    riscv64-linux-gnu-gcc -std=c11 -Wall -Wextra -Werror -Wno-cast-function-type \
        -static -march=rv64gc \
        $ffi_cflags -I ../../backend ../../backend/closure_runtime.c \
        closure_runtime_tests.c -o closure_runtime_rv64.exe $ffi_libs
    qemu-riscv64 ./closure_runtime_rv64.exe
else
    echo "skipped RV64 runtime execution: cross-compiler or QEMU not found"
fi
