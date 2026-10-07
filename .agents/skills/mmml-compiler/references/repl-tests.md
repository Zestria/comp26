# CLI, Cram и REPL-тесты

## Что уже существует

`MMML/bin/run.t` — Cram test LLVM demo, не interactive REPL.
Текущий `compiler.ml` умеет `--parse` и `--parse-expression` с stdin/файлом.
Не выдумывай `--repl`, `--anf`, prompt или печать вычисленного значения.
Запуск OCaml `utop` для модулей разработчика тоже не является REPL MiniML.

Источники Dune, проверено 2026-10-07:
[Cram syntax](https://dune.readthedocs.io/en/stable/reference/cram.html),
[cram stanza](https://dune.readthedocs.io/en/stable/reference/dune/cram.html).
`.t` содержит команды с префиксом двух пробелов и `$`; следующие строки
с двумя пробелами — ожидаемый вывод. `>` продолжает команду (например heredoc),
а `[1]` фиксирует ненулевой exit code. Cram сравнивает transcript и создаёт
`.corrected` при несовпадении. `dune promote` обновляет ожидания: сначала
прочитай diff и проверь поведение, не принимай изменения вслепую.

## CLI transcript для текущего MMML

В отдельном тестовом каталоге укажи `(cram (deps ../compiler.exe))`, если
каталог находится непосредственно внутри MMML. Не дублируй existing stanza
в `MMML/bin/dune`; там уже объявлены compiler и runtime. Если версия Dune
требует включения cram, проверь существующую конфигурацию и её документацию.
Тест должен действительно входить в `dune test -p MMML`.
Внутри sandbox запускай executable через declared dependency, а не nested dune.

Пример команд для будущего `.t` (ожидаемый AST получи из реального CLI и
проверь вручную; ниже deliberately нет выдуманного AST transcript):

```sh
printf '%s\n' 'let x = 1' | ../compiler.exe --parse
printf '%s\n' '1 + 2 * 3' | ../compiler.exe --parse-expression
printf '%s\n' 'let x =' | ../compiler.exe --parse
```

Последний случай должен проверить diagnostic и exit 1. Также покрывай
stdin/file equivalence, пустой файл, invalid token, EOF посреди выражения,
несовместимые опции (exit 2 от Arg), CPS factorial/fibonacci. Для ошибок
проверяй stdout и stderr отдельно, когда это часть контракта:

```text
  $ printf '%s\n' 'let x =' | ../compiler.exe --parse >out.txt 2>err.txt
  [1]
  $ test ! -s out.txt
  $ test -s err.txt
```

Этот короткий пример проверяет разделение потоков и status, но не текст
диагностики; отдельный snapshot `cat err.txt` должен содержать проверенные
ожидания. Не используй `|| true` и не пропускай ошибки пайплайном в `grep`.

## Когда появится настоящий REPL

Сначала установи его контракт: разделитель phrases, multiline input,
prompt при tty/pipe, EOF, состояния окружения и exit codes. `;;` — стандартный
разделитель OCaml toplevel, но не автоматически требование MiniML.

Проверяй несколько phrases в одном процессе, чтобы тестировать сохранение
bindings; запуск отдельного процесса для каждой строки не проверяет состояние.
Нужные сценарии: definition → use; shadowing; syntax/type error → следующая
валидная phrase; рекурсивная функция; multiline phrase; EOF на пустом и
незавершённом вводе. При ошибке проверь, остаётся ли прежнее окружение согласно
выбранному контракту. Передавай script через heredoc или файл, фиксируй stdout,
stderr и status. Не добавляй sleeps для синхронизации.

Pipe-тест проверяет batch-ввод. Если нужно проверить именно tty prompt,
line editing или interactive flushing, используй PTY harness с ограниченным
timeout и чтением до наблюдаемого prompt; Cram с обычным stdin этого не докажет.

Стабилизируй только непредсказуемые детали (например временный путь).
Не удаляй широким фильтром ошибки, имена переменных, IR или весь diagnostic.
Очередность names в map/set делай детерминированной, когда это часть печати.
Golden output и semantic properties дополняют друг друга: transcript ловит
изменение CLI, QCheck проверяет семейство входов.
