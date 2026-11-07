Снимки ассемблера проверяются всегда, даже когда в CI нет QEMU.
Файлы эталонов хранят весь вывод, включая слоты, ветвления и замыкания.

  $ for name in simple fack fibk; do
  >   ./snapshot.exe "$name.src" > "$name.actual.s" || exit 1
  >   diff "$name.expected.s" "$name.actual.s" || exit 1
  > done
