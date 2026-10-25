#!/bin/sh
# Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors
# SPDX-License-Identifier: LGPL-3.0-or-later
set -eu

case "${1:-}" in
    codegen|runtime) mode="$1" ;;
    *) echo "usage: docker_runner.sh <codegen|runtime>" >&2; exit 2 ;;
esac

docker="${DOCKER:-docker}"
if ! command -v "$docker" >/dev/null 2>&1; then
    echo "Docker CLI not found; add it to PATH or set DOCKER to its full path" >&2
    exit 1
fi
if ! "$docker" info >/dev/null; then
    echo "Docker is unavailable; start Docker Desktop or the Docker daemon" >&2
    exit 1
fi

image="shmel-riscv-tests:local"
# Docker caches the toolchain layer. Reading the Dockerfile from stdin avoids
# sending generated programs or the repository as build context.
"$docker" build --quiet --tag "$image" - < Dockerfile >&2

archive=$(mktemp)
trap 'rm -f "$archive"' EXIT HUP INT TERM
# Dune may provide symlinks; dereference them and omit macOS archive metadata.
case "$mode" in
    codegen)
        COPYFILE_DISABLE=1 tar --format=ustar -chf "$archive" run_riscv.sh harness.c tests.s
        ;;
    runtime)
        COPYFILE_DISABLE=1 tar --format=ustar -chf "$archive" run_riscv.sh closure_runtime_tests.c \
            -C ../../backend closure_runtime.c closure_runtime.h
        ;;
esac

# Stream files rather than mounting Dune's build directory: this also works
# with remote Docker daemons and paths inside a Dune sandbox.
"$docker" run --rm -i \
    -e "SHMEL_QEMU_TIMEOUT=${SHMEL_QEMU_TIMEOUT:-60}" \
    "$image" sh -ec 'cd /tmp; tar -xf -; exec sh run_riscv.sh "$@"' sh "$mode" \
    < "$archive"
