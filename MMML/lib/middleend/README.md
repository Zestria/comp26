# ANF middleend MMML

Библиотека `MMML.middleend` переводит `Ast.expr` и `Ast.program` в
нетипизированный ANF. В Dune добавьте `(libraries MMML.middleend)`;
wrapped-модули доступны через `Middleend`.

## Представление

- `Anf.imm`: константы, локальные IDs, внешние ссылки и известные встроенные значения.
- `Anf.compound`: одна операция с атомарными операндами; тела lambda и ветви if являются термами.
- `Anf.term`: цепочка let/let rec с конечным compound.
- `Anf.program`: упорядоченные definitions с полноценными ANF rhs.

Локальные IDs сравниваются по `stamp`; `hint` помогает читать диагностику.
Все binders одной программы имеют разные stamps. Внешнее имя `anf0` не
может совпасть с временным локальным ID. Счётчик принадлежит одному запуску.
`Check` проверяет scope и уникальность IDs, но не выводит типы.

## Использование

Следующий пример исполняется как `MMML/test/anf_usage_example.exe`:

```ocaml
open Middleend

let () =
  let source = "fun c -> h (if c then f 1 else g 2)" in
  let ast =
    match Parser_menhir.parse_expression source with
    | Ok ast -> ast
    | Error error -> failwith (Format.asprintf "%a" Error_monad.pp_error error)
  in
  let ir =
    match Normalize.expression ast with
    | Ok ir -> ir
    | Error error -> failwith (Format.asprintf "%a" Normalize.pp_error error)
  in
  match Check.term ~externals:[ "h"; "f"; "g" ] ir with
  | Ok () -> Format.printf "%a@." Pretty_printer.pp_term ir
  | Error error -> failwith (Format.asprintf "%a" Check.pp_error error)
```

Для программы используйте `Normalize.program`, `Check.program` и
`Pretty_printer.pp_program`. Имена из предыдущих definitions доступны
последующим; rhs обычного определения видит старую привязку при shadowing.

`Normalize.expression ~bindings` и `Normalize.program ~bindings` заменяют
начальное окружение. По умолчанию оно содержит стандартные операторы.
Например, `~bindings:(("+", Anf.External "custom_add") ::
List.remove_assoc "+" Anf.default_bindings)` отключает primitive-распознавание
для `+`. Неизвестные свободные имена сохраняются как `External`; checker
принимает перечень разрешённых внешних ссылок явно. В реальной программе
внешние ссылки должны обеспечиваться окружением выполнения.

## Семантика преобразования

Application вычисляет сначала функцию, затем аргумент, затем выполняет
одноаргументный вызов. `f a b` сохраняет промежуточное применение `f a`
до вычисления b. Тело lambda выполняется только при вызове функции.

Обычный let вычисляет rhs в старом окружении. Рекурсивная функция видит
собственное имя и в теле, и после определения. Поддерживается только
`let rec f = fun ...`; non-lambda rhs и recursive wildcard возвращают явную
ошибку нормализации. Этот проход не является проверкой типов.

Насыщенные известные `&&`/`||`, включая простые aliases встроенного
значения, переводятся в if. В `false && dangerous ()` и
`true || dangerous ()` опасная часть не выполняется. Затенённые операторы,
неизвестные higher-order функции и уже вычисленные частичные применения
используют обычный strict application. Например, `let both = (&&) false in
both (dangerous ())` вычисляет аргумент перед вызовом `both`.

Для `h (if c then f x else g y)` общее продолжение представлено один раз:

```ocaml
let r = if c then f x else g y in
h r
```

Это схема; diagnostic printer явно различает IDs, externals и builtins,
и его вывод не предназначен для повторного разбора frontend-парсером.

ANF допускает конечный compound, поэтому не создаёт
`let t = f x in t`. Атомарные aliases локальных значений/констант устраняются.
Внешние lookup сохраняются при необходимости фиксации порядка ошибок.
Вычисление `let _ = print_int 1 in 42` сохраняет печать, а
`let _ = 1 / 0 in 42` — ошибку. Проход не удаляет произвольные вычисления.

## Проверки

Из корня репозитория, в opam switch проекта:

```sh
opam exec -- dune exec MMML/test/anf_ir_test.exe
opam exec -- dune exec MMML/test/anf_normalize_test.exe
opam exec -- dune exec MMML/test/anf_semantics_test.exe
opam exec -- dune exec MMML/test/anf_qcheck.exe -- --seed 42
opam exec -- dune exec MMML/test/anf_usage_example.exe
opam exec -- dune b -p MMML
opam exec -- dune test -p MMML
```

Тестовые AST/ANF interpreter обходят разные деревья независимо и сравнивают
значение, trace печати и runtime error. CPS-факториал 5 и Fibonacci 10 должны
давать 120 и 55. QCheck генерирует ограниченные типизированные terms;
shrinker сохраняет типы и scope, включая shadowing. Fuel exhaustion считается
неопределённым вычислением и никогда не успешным совпадением.

CLI, closure conversion, lambda lifting и backend не входят в эту библиотеку.
Использование ANF через `--stop-after anf` требует отдельной интеграции CLI.
