#!/bin/sh
set -eu

runner=local
if [ "${1:-}" = --runner ]; then
    if [ "$#" -lt 3 ]; then
        echo "usage: sh shmel/run.sh [--runner local|docker] input.ml" >&2
        exit 2
    fi
    runner="$2"
    shift 2
fi
if [ "$#" -ne 1 ]; then
    echo "usage: sh shmel/run.sh [--runner local|docker] input.ml" >&2
    exit 2
fi
case "$runner" in
    local|docker) ;;
    *) echo "unknown runner: $runner" >&2; exit 2 ;;
esac
case "$1" in
    /*) input="$1" ;;
    *) input="$PWD/$1" ;;
esac
repo=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM
dune exec --root "$repo" shmel/compiler.exe -- -o "$work/program.s" "$input"
cp "$repo/shmel/test/riscv/run_riscv.sh" \
    "$repo/shmel/test/riscv/docker_runner.sh" \
    "$repo/shmel/test/riscv/Dockerfile" "$work/"
cd "$work"
if [ "$runner" = docker ]; then
    sh docker_runner.sh program "$repo/shmel/backend"
else
    sh run_riscv.sh program "$repo/shmel/backend" program.s
fi
