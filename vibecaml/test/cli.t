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

Расширения ещё не реализованы: флаг даёт ошибку до чтения входа, поэтому
несуществующий файл не мешает.

  $ vibecamlc --ext-glued-keywords missing.ml
  vibecaml: расширение --ext-glued-keywords пока не реализовано
  [1]

  $ vibecamlc --ext-operator-declarations missing.ml
  vibecaml: расширение --ext-operator-declarations пока не реализовано
  [1]

  $ vibecamlc --parser menhir --ext-glued-keywords missing.ml
  vibecaml: расширение --ext-glued-keywords не поддерживается в menhir-парсере
  [1]

  $ vibecamlc --parser menhir --ext-operator-declarations missing.ml
  vibecaml: расширение --ext-operator-declarations не поддерживается в menhir-парсере
  [1]

С двумя флагами сообщение про первое расширение из Config, порядок в командной
строке не важен.

  $ vibecamlc --ext-operator-declarations --ext-glued-keywords missing.ml
  vibecaml: расширение --ext-glued-keywords пока не реализовано
  [1]
