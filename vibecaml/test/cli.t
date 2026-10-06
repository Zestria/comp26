Программа для разбора.

  $ cat > prog.ml <<'ML'
  > let x = 1 + 2 * 3
  > let rec f a b = if a then b else f a (b, b)
  > ML

Разбор файла нисходящим парсером (по умолчанию) и menhir печатает программу
принтером.

  $ vibecamlc --stop-after parsing prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

  $ vibecamlc -parsing menhir --stop-after parsing prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

Без файла программа читается из stdin.

  $ vibecamlc --stop-after parsing < prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

`--dump-ast` печатает только дерево, `--dump-pp` --- только программу, оба
флага --- сначала дерево, потом программу.

  $ echo 'let x = f 1' > small.ml

  $ vibecamlc --dump-ast small.ml
  [(Structure_value (Non_recursive,
      { binding_pattern = (Pat_var "x");
        binding_expression =
        (Expr_app ((Expr_ident "f"), (Expr_constant (Const_int 1)))) },
      []))
    ]

  $ vibecamlc --dump-pp small.ml
  let x = f 1

  $ vibecamlc --dump-pp --dump-ast small.ml
  [(Structure_value (Non_recursive,
      { binding_pattern = (Pat_var "x");
        binding_expression =
        (Expr_app ((Expr_ident "f"), (Expr_constant (Const_int 1)))) },
      []))
    ]
  let x = f 1

Одиночный `-` тоже означает stdin.

  $ vibecamlc --stop-after parsing - < prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

  $ vibecamlc -parsing menhir --stop-after parsing - < prog.ml
  let x = 1 + 2 * 3
  let rec f a b = if a then b else f a (b, b)

Ошибка разбора.

  $ echo 'let = 1' | vibecamlc
  vibecaml: 1:5: ожидался паттерн
  [1]

  $ echo 'let = 1' | vibecamlc -parsing menhir
  vibecaml: 1:5: синтаксическая ошибка у "="
  [1]

Несуществующий файл.

  $ vibecamlc missing.ml
  vibecaml: missing.ml: No such file or directory
  [1]

Проверка опции `--ext-glued-keywords` для нисходящего парсера:
корректная обработка слипшегося ключевого слова и идентификатора.

  $ echo 'letx = 1 letrecf n = if n <= 1 then 1 else n * f (n - 1)' > glued.ml

  $ vibecamlc --stop-after parsing --ext-glued-keywords glued.ml
  let x = 1
  let rec f n = if n <= 1 then 1 else n * f (n - 1)

  $ echo 'let y = letx' > ident.ml

  $ vibecamlc --stop-after parsing ident.ml
  let y = letx

Опция `--ext-operator-declarations` для восходящего парсера не реализована

  $ echo 'let x = 1' > ext.ml

  $ vibecamlc --ext-operator-declarations ext.ml
  vibecaml: расширение --ext-operator-declarations пока не реализовано
  [1]

Восходящий парсер не поддерживает любые из расширений

  $ vibecamlc -parsing menhir --ext-glued-keywords ext.ml
  vibecaml: расширение --ext-glued-keywords не поддерживается в menhir-парсере
  [1]

  $ vibecamlc -parsing menhir --ext-operator-declarations ext.ml
  vibecaml: расширение --ext-operator-declarations не поддерживается в menhir-парсере
  [1]

С двумя флагами сообщение про первое расширение из Config, порядок в командной
строке не важен.

  $ vibecamlc --ext-operator-declarations --ext-glued-keywords ext.ml
  vibecaml: расширение --ext-operator-declarations пока не реализовано
  [1]

  $ vibecamlc -parsing menhir --ext-operator-declarations --ext-glued-keywords ext.ml
  vibecaml: расширение --ext-glued-keywords не поддерживается в menhir-парсере
  [1]

Флаги из ТЗ. `-parsing topdown` --- нисходящий парсер, он же по умолчанию.

  $ vibecamlc -parsing topdown --stop-after parsing small.ml
  let x = f 1

  $ vibecamlc -parsing lalr small.ml
  vibecaml: неизвестный парсер: lalr (ожидается topdown или menhir)
  [1]

  $ vibecamlc --parser menhir small.ml
  vibecaml: неизвестный аргумент: --parser
  [1]

`--stop-after parsing` печатает программу после разбора.

  $ vibecamlc --stop-after parsing small.ml
  let x = f 1

  $ vibecamlc --stop-after typing small.ml
  vibecaml: неизвестная стадия: typing (ожидается parsing, cc, ll или anf)
  [1]

  $ vibecamlc --stop-after
  vibecaml: после --stop-after ожидается значение
  [1]

  $ vibecamlc --dump-ast --stop-after ll small.ml
  vibecaml: --dump-ast и --dump-pp работают только с --stop-after parsing
  [1]

`-o` пишет результат в файл, `-o -` --- в stdout.

  $ vibecamlc --stop-after parsing -o out.ml small.ml
  $ cat out.ml
  let x = f 1

  $ vibecamlc --stop-after parsing -o - small.ml
  let x = f 1

  $ vibecamlc small.ml -o
  vibecaml: после -o ожидается значение
  [1]

  $ vibecamlc -o missing/out.ml small.ml
  vibecaml: missing/out.ml: No such file or directory
  [1]

Значение флага не может быть другим флагом.

  $ vibecamlc -o --stop-after ll small.ml
  vibecaml: после -o ожидается значение
  [1]
