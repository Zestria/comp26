# Контекст репозитория

Снимок структуры проверен 2026-10-07; это ориентир, не замена чтению кода.
Все пути ниже относительны корню репозитория.

| Файлы | Назначение |
|---|---|
| `AGENTS.md`, `dune-project` | Требования курса, packages `MMML` и `demo`, зависимости |
| `MMML/compiler.ml`, `MMML/dune` | CLI и executable `MMML/compiler.exe` |
| `MMML/lib/frontend/ast.ml` | Нетипизированный AST, приложения каррированы |
| `MMML/lib/frontend/topdown_parser.ml` | Нисходящий парсер |
| `MMML/lib/frontend/lexer.mll`, `parser.mly`, `parser_menhir.ml` | ocamllex, Menhir и входные точки |
| `MMML/lib/frontend/pretty_printer.ml` | Печать MiniML, не то же самое, что `Ast.show_expr` |
| `MMML/test/dune`, `parser_menhir_qcheck.ml`, `lexer_qcheck.ml` | Тесты и QCheck properties |
| `MMML/test_qc/test_qc.ml` | Round-trip нисходящего парсера с production printer |
| `MMML/bin/run.t`, `MMML/bin/dune` | Cram для LLVM demo, зависимости runtime и executable |
| `.ocamlformat` | `version=0.29.0`, `profile=janestreet` |

CLI имеет `--parse` (программа) и `--parse-expression` (выражение), читает
stdin или один файл. Печатает представление AST; ошибка frontend даёт exit 1.
Без опций запускается LLVM demo, создающий `out.ll`. Это не интерактивный REPL.
`public_name` executable — `mml-compiler`, package — `MMML`.

AST включает константы int/bool/unit, переменные, lambda, application, if и let;
patterns пока только variable/wildcard. Не предполагай готовое представление
типизированного AST, ANF, полных match-patterns или RISC-V backend.
Примеры в справках показывают подход и не являются такими модулями проекта.

`generate_opam_files true`: зависимости изменяют в `dune-project`,
а не вручную в сгенерированном `MMML.opam`.
Профиль форматирования Jane Street не означает зависимость от Core/Base.

При подготовке скилла локально доступны OCaml 4.14.2 и Dune 3.20.2,
тогда как `dune-project` требует Dune language 3.24. Не понижай проект ради
локального теста. Если среда не подходит, сообщи это и отдели проверки
самостоятельных примеров от проверки package и CI.
