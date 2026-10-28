# RISC-V: тулчейн, запуск, отладка

Контракт с рантаймом — `adr/0003-riscv-contract.md`.

## Установка

    sudo apt install binutils-riscv64-linux-gnu gdb-multiarch qemu-user

Тесты `test/riscv/qemu.t` включаются сами, когда в PATH есть `qemu-riscv64`.

## Запуск

    vibecamlc prog.ml -o prog.s
    scripts/run.sh prog.s        # или: make run PROG=prog.s

`scripts/run.sh` собирает `prog.s` и `runtime/runtime.s` через `as` и `ld` и
запускает результат под QEMU.

## Отладка

В одном терминале:

    scripts/run.sh -g 1234 prog.s

QEMU ждёт отладчик, а бинарь с символами кладётся в `./a.out`. Во втором:

    gdb-multiarch a.out -x tests/.gdbinit

`.gdbinit` подключается к `:1234`, показывает регистры и ставит точку
останова на `_start`. Дальше: `si`/`ni` — шаг по инструкции с заходом в вызов
и без, `b vc_main`, `c`, `info registers a0`, `x/8xg $sp` — 8 слов со стека.
