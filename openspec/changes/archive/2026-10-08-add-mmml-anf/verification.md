# Проверка реализации

Дата: 2026-10-08. Среда: opam switch `fp25`, OCaml 5.5.1,
Dune 3.24.2, ocamlformat 0.29.0, LLVM/Clang 18.1.8.
Команды запускаются через `opam exec --`.

## Результаты

- `ocamlformat --check MMML/lib/middleend/*.ml MMML/lib/middleend/*.mli MMML/test/anf_*.ml`: успешно.
- `dune b -p MMML`: успешно.
- Новые `anf_ir_test`, `anf_normalize_test`, `anf_semantics_test`,
  `anf_usage_example`: успешно.
- `dune exec MMML/test/anf_qcheck.exe -- --seed 42`: успешно;
  1000 случаев сохранения scope/семантики/детерминизма и 200 проверок
  корректности домена shrinker.
- `dune test -p MMML`: код завершения 1, два существовавших до ANF
  расхождения Cram. В `MMML/llvm/1.t` LLVM печатает `declare dso_local`
  вместо `declare`; в `MMML/bin/run.t` Clang дополнительно печатает
  предупреждение о замене target triple. Исполняемый пример выдаёт `F`.
  Оба diff совпадают с проверкой среды до реализации ANF
  (индексы `0f68892..ac98650` и `27093be..7a58860`).
  Остальные существующие тесты прошли при исходной проверке среды;
  повторный запуск пакета успешно выполнил новые ANF-тесты.
- `git diff --check`: успешно.

Полный набор тестов пакета не считается полностью прошедшим. LLVM transcript
fixtures не менялись в рамках этого изменения.

## Покрытие сценариев спецификации

| Требование | Проверки |
| --- | --- |
| Expressions/programs, CPS, empty program | `anf_normalize_test`, `anf_semantics_test`: CPS factorial 120, Fibonacci 55, пустая программа |
| Atomic operands, nested application | Типы IR, `anf_ir_test`, структурные проверки `anf_normalize_test` |
| Function/argument/call order, curried effect | `anf_semantics_test`: фиксированные последовательности эффектов |
| Conditional execution, single continuation | `anf_normalize_test`: структура и линейный рост; `anf_semantics_test`: обе ветви |
| Short circuit, required/skipped RHS, aliases | `anf_normalize_test`, `anf_semantics_test`: effects, division error, nontermination |
| Shadowed arithmetic/logical operators | `anf_normalize_test`, `anf_semantics_test`: обычные unary applications и строгие аргументы |
| Lexical scope, shadowing, free anf0 | `anf_normalize_test`, `anf_semantics_test`, checker и QCheck |
| Lambda/recursion, unsupported recursive forms | `anf_normalize_test`, `anf_semantics_test`, `anf_ir_test` |
| Ordered top-level definitions | `anf_normalize_test`, `anf_semantics_test`: старое окружение RHS и порядок инициализации |
| Tail operations, arithmetic primitives, wildcard | `anf_normalize_test`, `anf_semantics_test`: структура, эффекты и ошибки |
| Reproducibility, printer, invalid scope | `anf_ir_test`, `anf_normalize_test`, QCheck |
| Semantic preservation, generated terms, fuel | Независимые AST/ANF interpreters, `anf_semantics_test`, `anf_qcheck` |

Изменения ограничены новой библиотекой `MMML.middleend`, её документацией,
тестами и добавлением тестовых Dune stanzas. Код frontend, compiler executable,
существующие тесты и CLI-опции не изменялись. Пример API компилируется и
исполняется в `anf_usage_example`.
