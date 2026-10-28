# MMML

Учебный компилятор подмножества OCaml в общем Dune-проекте `comp26`.
Пакет — `MMML`; команда запуска — `dune exec MMML/compiler.exe`.

Реализованы frontend и closure conversion непосредственно `Ast.program → Ast.program`.
Захваты передаются дополнительными параметрами и фиксируются частичным применением.
ANF реализуется отдельно после closure conversion. Lambda-lifting/hoisting, вывод
типов и RISC-V также остаются отдельными этапами. Без аргументов compiler выполняет
LLVM-demo, не компилирующий входную программу MiniML.

## Документация для работы

- [Передача проекта в OpenSpec и начало следующей задачи](../openspec/README.md).
- [Проверенное состояние, архитектура и известные пробелы](docs/current-state.md).
- [Требования заданий, порядок изменений и открытые решения](docs/roadmap.md).
- [Closure conversion: API, IR, ограничения и проверки](docs/closure-conversion.md).
- [Навык разработки компилятора](../.agents/skills/mmml-compiler-dev/SKILL.md).
- [Практики OCaml](../.agents/skills/mmml-compiler-dev/references/ocaml-practices.md).
- [Проектирование проходов компилятора](../.agents/skills/mmml-compiler-dev/references/compiler-passes.md).
- [Проверка корректности и производительности](../.agents/skills/mmml-compiler-dev/references/validation.md).

## Команды из корня репозитория

```sh
dune build -p MMML
dune test -p MMML
dune exec MMML/test/closure_usage_example.exe
dune build -p MMML @check @runtest @install
printf 'let id x = x\n' | dune exec MMML/compiler.exe -- --parse
printf '1 + 2 * 3\n' | dune exec MMML/compiler.exe -- --parse-expression
dune exec MMML/compiler.exe -- --parse path/to/input.ml
dune build @MMML/benchmark/benchmark --force
```

Для сборки нужны зависимости пакета, включая OCaml 5.5.1, Dune ≥ 3.24
и OCaml bindings LLVM `18-shared`. Унаследованные cram-тесты используют
`clang-18` и `opt-18`. Текущий CLI печатает структурное представление AST.
Опции `--stop-after`, `-parsing` и `-o` пока не реализованы.

Общие требования преподавателя находятся в [AGENTS.md](../AGENTS.md).
Директории других команд — отдельные компиляторы и не входят в scope MMML.
