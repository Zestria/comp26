#!/usr/bin/env bash
qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 $1
