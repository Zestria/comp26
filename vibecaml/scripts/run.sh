#!/bin/sh
# Собрать программу на RISC-V вместе с рантаймом и запустить под QEMU.
#
#   scripts/run.sh [-g PORT] prog.s [runtime.s]
#
# -g PORT --- ждать gdb на порту (см. docs/riscv.md). Рантайм по умолчанию ---
# runtime/runtime.s рядом со скриптом. Код возврата --- код программы.
set -eu

: "${RISCV_PREFIX:=riscv64-linux-gnu-}"
gdb_port=
if [ "${1:-}" = -g ]; then
  gdb_port=$2
  shift 2
fi
if [ $# -lt 1 ] || [ $# -gt 2 ]; then
  echo "usage: $0 [-g PORT] prog.s [runtime.s]" >&2
  exit 2
fi
program=$1
runtime=${2:-$(dirname "$0")/../runtime/runtime.s}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
"${RISCV_PREFIX}as" -march=rv64gc -o "$work/program.o" "$program"
"${RISCV_PREFIX}as" -march=rv64gc -o "$work/runtime.o" "$runtime"
"${RISCV_PREFIX}ld" -o "$work/a.out" "$work/program.o" "$work/runtime.o"

set -- -L /usr/riscv64-linux-gnu -cpu rv64
if [ -n "$gdb_port" ]; then
  set -- "$@" -g "$gdb_port"
  # gdb нужен бинарь с символами, а временный каталог исчезнет.
  cp "$work/a.out" ./a.out
  echo "a.out: gdb-multiarch a.out -x tests/.gdbinit" >&2
fi
qemu-riscv64 "$@" "$work/a.out"
