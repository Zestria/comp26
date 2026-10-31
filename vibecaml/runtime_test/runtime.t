Пять C-драйверов проверяют runtime без Codegen. Сборка и запуск входят
в dune runtest; вывод сравнивается целиком. У двух проверок ожидается
завершение через vc_fail с кодом 1, у остальных --- обычный код 0.

  $ for number in 1 2 3 4 5; do
  >   riscv64-linux-gnu-gcc -O3 -nostartfiles ../runtime/runtime.s "case$number/main.c" -o "case$number.exe" || exit 1
  >   expected_status=0
  >   case "$number" in 1|5) expected_status=1 ;; esac
  >   status=0
  >   qemu-riscv64 -L /usr/riscv64-linux-gnu/ "./case$number.exe" > actual.txt 2>&1 || status=$?
  >   test "$status" -eq "$expected_status" || exit 1
  >   diff "case$number/expected.txt" actual.txt || exit 1
  > done
