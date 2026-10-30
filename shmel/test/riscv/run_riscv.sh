#!/bin/sh
# Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors
# SPDX-License-Identifier: LGPL-3.0-or-later
set -eu

run_qemu() {
    if command -v timeout >/dev/null 2>&1; then
        timeout "${SHMEL_QEMU_TIMEOUT:-60}" qemu-riscv64 -cpu max "$@"
    else
        qemu-riscv64 -cpu max "$@"
    fi
}

case "${1:-}" in
    available)
        command -v riscv64-linux-gnu-gcc >/dev/null 2>&1 &&
            command -v qemu-riscv64 >/dev/null 2>&1
        ;;
    codegen)
        riscv64-linux-gnu-gcc -static -march=rv64gc harness.c tests.s -o tests.exe
        run_qemu ./tests.exe
        ;;
    runtime|integration|program|programs)
        mode="$1"
        backend="${2:-.}"
        build() {
            riscv64-linux-gnu-gcc -std=c11 -Wall -Wextra -Werror -Wno-cast-function-type \
                -static -march=rv64gc -mabi=lp64d -I "$backend" \
                "$backend/closure_runtime.c" "$backend/closure_invoke.S" "$@" -o program_rv64.exe
        }
        case "$mode" in
            runtime) build closure_runtime_tests.c ;;
            integration) build integration.c integration.s ;;
            program) build "$backend/print_runtime.c" "${3:-program.s}" ;;
            programs)
                directory="${3:-programs}"
                count=0
                while IFS= read -r name; do
                    build "$backend/print_runtime.c" "$directory/$name.s"
                    if ! run_qemu ./program_rv64.exe > "$directory/$name.out"; then
                        echo "program $name: execution failed" >&2
                        exit 1
                    fi
                    if ! cmp -s "$directory/$name.expected" "$directory/$name.out"; then
                        echo "program $name: output differs from Eval" >&2
                        diff -u "$directory/$name.expected" "$directory/$name.out" >&2 || true
                        exit 1
                    fi
                    count=$((count + 1))
                done < "$directory/manifest"
                echo "$count whole-program cases run on QEMU, 0 failed"
                exit 0
                ;;
        esac
        run_qemu ./program_rv64.exe
        ;;
    *)
        echo "usage: run_riscv.sh <codegen|runtime|integration|program|programs> [backend-directory] [assembly-or-directory]" >&2
        exit 2
        ;;
esac
