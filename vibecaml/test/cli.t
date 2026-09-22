Программа для разбора.

  $ cat > prog.ml <<'ML'
  > let x = 1 + 2 * 3
  > let rec f a b = if a then b else f a (b, b)
  > ML

Разбор файла нисходящим парсером (по умолчанию) и menhir печатает программу
принтером.

  $ vibecamlc prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

  $ vibecamlc --parser menhir prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

Без файла программа читается из stdin.

  $ vibecamlc < prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

Одиночный `-` тоже означает stdin.

  $ vibecamlc - < prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

  $ vibecamlc --parser menhir - < prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

Ошибка разбора.

  $ echo 'let = 1' | vibecamlc
  vibecaml: 1:5: ожидался паттерн
  [1]

  $ echo 'let = 1' | vibecamlc --parser menhir
  vibecaml: 1:5: синтаксическая ошибка у "="
  [1]

Несуществующий файл.

  $ vibecamlc missing.ml
  vibecaml: missing.ml: No such file or directory
  [1]
