# QCheck: генераторы и свойства компилятора

## API и существующие тесты

Официальные исходники:
[QCheck.mli](https://github.com/c-cube/qcheck/blob/main/src/core/QCheck.mli),
[runner](https://github.com/c-cube/qcheck/blob/main/src/runner/QCheck_base_runner.mli).
Проверено 2026-10-07; установленный `.mli` — источник истины для версии среды.
Проект использует `QCheck`: `Gen` строит generator, `make` объединяет его
с printer/shrinker, `Test.make` задаёт property.
`QCheck_runner.run_tests_main` принимает CLI, включая `--seed`.
Не смешивай интерфейсы QCheck и QCheck2: у QCheck2 shrinking встроен в generator.
В рамках задачи не мигрируй весь тестовый набор между этими API.

Прочитай `MMML/test/parser_menhir_qcheck.ml`: там ограниченный генератор,
shrinker и тестовый полностью скобочный serializer, независимый от production
pretty-printer. `MMML/test_qc/test_qc.ml` проверяет другой round-trip.
`[@@deriving qcheck]` полезен, но сам по себе не гарантирует closed/well-typed AST.

## Выбор свойств (адаптация для MMML)

| Стадия | Свойство | Ограничение |
|---|---|---|
| Lexer | locations и токены не зависят от разбиения входа на chunks | trivia добавляй только между токенами |
| Parser | `parse(test_serialize(ast)) = ast` | serializer независим от production printer |
| Printer | `parse(pretty(ast)) = ast` | отдельно фиксируй precedence и associativity |
| Два parser | одинаковый AST для общего синтаксиса | extensions тестируй отдельно с опцией |
| Parser errors | malformed input отвергается, есть позиция | suffix не должен случайно быть валидным |
| Type inference | известные typed terms имеют ожидаемые типы | сравнивай type vars с точностью до renaming |
| ANF/CC | результат, scope и эффекты сохраняются | bounded terminating и корректно типизированные terms |

Два одинаково ошибающихся parser могут совпасть; differential property
дополняй ожидаемыми AST для precedence, CPS factorial/fibonacci и ошибок.
Отдельно тестируй полное потребление входа.

## Генераторы и shrinking

Ограничивай размер и ветвление рекурсивного AST: depth bound при двух/трёх
детях даёт экспоненциальный максимальный размер. Для крупных деревьев дели
budget между детьми. Генератор идентификаторов исключает keywords и соблюдает
лексику проекта; keywords с прилипшим именем проверяй целевыми случаями.

Для parser property допустимы свободные и нетипизированные выражения.
Для interpreter/backend property передавай environment в generator,
генерируй переменную из доступных bindings, body let в расширенном окружении.
Для typed property выбирай конструкторы по целевому типу. Ограничивай recursion
или fuel; timeout/inconclusive не считай успешным совпадением результатов.

Shrinker уменьшает дерево, сохраняя домен свойства. Удаление binder может
оставить свободное имя, а замена if его condition нарушить тип. Для closed/typed
тестов используй scope/type-aware shrinking или фильтр корректности кандидатов.
Задай printer с исходным term, seed и достаточной диагностикой различия.
Не заменяй все неожиданные исключения `true`, не отвергай большую часть samples
через `assume`; по возможности генерируй допустимые значения сразу.

## Исполняемый пример

[anf_qcheck.ml](examples/anf_qcheck.ml) проверяет сохранение значения и scope
на closed terms игрушечного языка из [anf_example.ml](examples/anf_example.ml).
Имена намеренно иногда совпадают с префиксом fresh names; let порождает
shadowing. Shrinker фильтрует кандидаты по closedness. Это образец организации
теста, а не доказательство корректности MMML.

Из корня репозитория, не добавляя executable в package:

```sh
task_dir=$(mktemp -d)
cp .agents/skills/mmml-compiler/references/examples/anf_*.ml "$task_dir/"
(cd "$task_dir" && ocamlfind ocamlc -package qcheck -linkpkg anf_example.ml anf_qcheck.ml -o anf_qcheck.exe && ./anf_qcheck.exe --seed 42)
```

Рабочий dune test регистрируй по образцу `MMML/test/dune`, с `(package MMML)`
и библиотеками `MMML.frontend qcheck`. Для воспроизведения существующего теста:

```sh
dune exec MMML/test/parser_menhir_qcheck.exe -- --seed 42
dune test -p MMML
```

`MMML/test_qc/test_qc.ml` сейчас вызывает `run_tests`, а не `run_tests_main`;
не обещай ему поддержку `--seed` без изменения runner.
Сохраняй минимальный найденный контрпример как фиксированную регрессию.
Число samples выбирай по стоимости свойства; 1000 запусков не доказывают
корректность и не заменяют направленные случаи.
