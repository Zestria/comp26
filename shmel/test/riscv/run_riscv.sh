#!/bin/sh
# Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors
# SPDX-License-Identifier: LGPL-3.0-or-later
set -eu

run_qemu() {
    if command -v timeout >/dev/null 2>&1; then
        timeout "${SHMEL_QEMU_TIMEOUT:-60}" qemu-riscv64 "$@"
    else
        qemu-riscv64 "$@"
    fi
}

case "${1:-}" in
    codegen)
        riscv64-linux-gnu-gcc -static -march=rv64gc harness.c tests.s -o tests.exe
        run_qemu -cpu max ./tests.exe
        ;;
    runtime|integration)
        mode="$1"
        backend="${2:-.}"
        if [ "$mode" = runtime ]; then
            set -- closure_runtime_tests.c
        else
            set -- integration.c integration.s
        fi
        riscv64-linux-gnu-gcc -std=c11 -Wall -Wextra -Werror -Wno-cast-function-type \
            -static -march=rv64gc -I "$backend" \
            "$backend/closure_runtime.c" "$backend/closure_invoke.S" "$@" \
            -o "${mode}_rv64.exe"
        run_qemu -cpu max "./${mode}_rv64.exe"
        ;;
    *)
        echo "usage: run_riscv.sh <codegen|runtime|integration> [backend-directory]" >&2
        exit 2
        ;;
esac
