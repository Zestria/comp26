#!/usr/bin/env bash

dune exec -p SchonfinkelML SchonfinkelML/compiler.exe -- $1 > /tmp/a.S
riscv64-linux-gnu-as -march=rv64gc /tmp/a.S -o /tmp/temp_object.schoml.o
ls SchonfinkelML/backend/*.c | xargs -I {} sh -c 'riscv64-linux-gnu-gcc -c "{}" -o "/tmp/$(basename "{}").schoml.o"'
riscv64-linux-gnu-gcc -nostartfiles /tmp/*.schoml.o -o a.out


