# Как поднять проект с нуля

Проверено на Ubuntu 22.04 с OCaml 5.5.1. На macOS отличается только шаг 2.

## 1. Клонирование

```
git clone ssh://ssh.sourcecraft.dev/mrvibecodic/comp26.git
cd comp26
git remote add upstream https://git.sourcecraft.dev/kakadu/comp26.git
git fetch upstream
```

Апстрим — репозиторий преподавателя, оттуда приезжают задания и правки CI.
Работа там идёт в ветке `master`, а не `main`:

```
git checkout main
git merge --ff-only upstream/master
git push origin main
```

## 2. opam

```
sudo apt install opam
opam init --bare --shell-setup
```

`--shell-setup` обязателен: без него opam не пропишет себя в `~/.profile`,
и в новом терминале `dune` не найдётся. apt заодно поставит системный
OCaml 4.13 — он нам не нужен, рабочий компилятор ставится на следующем шаге.

## 3. Компилятор

В CI используется OCaml **5.5.1**, локально нужен такой же:

```
opam switch create comp26 5.5.1
eval $(opam env --switch=comp26)
```

Сборка компилятора из исходников занимает 10–20 минут.

Если opam не достучится до репозитория пакетов — переключитесь на зеркало,
им же пользуется CI:

```
opam repo set-url default https://se.math.spbu.ru/opam
opam update
```

## 4. Зависимости

```
cd vibecaml
make deps
```

Ставит зависимости только пакета vibecaml, **включая dune** — отдельно его
устанавливать не нужно.

opam-файлы всех компиляторов курса лежат в корне проекта, а не в папках
пакетов, поэтому `make deps` указывает путь `../vibecaml.opam` явно. Если
вызвать `opam install . --deps-only` из `vibecaml/`, opam не найдёт пакет;
из корня — потянет зависимости `demo` вместе с LLVM 18.

## 5. Инструменты разработки

```
opam install ocamlformat.0.29.0 ocaml-lsp-server
opam pin add zanuda https://github.com/Kakadu/zanuda.git --yes
```

Версия ocamlformat должна совпадать с указанной в корневом `.ocamlformat`
(сейчас 0.29.0), иначе он откажется работать. Без него падает `make fmt-check`.
Zanuda в opam-репозитории нет, только через pin.

## 6. Проверка

Из `vibecaml/`:

```
make fmt-check && make release && make test && make lint
```

Всё зелёное — окружение готово. Полный список целей: `make help`.

## Грабли

**`dune: command not found` в новом терминале.** Не выполнен `eval $(opam env)`.
Лечится `opam init --shell-setup`.

**ocamlformat ругается на версию.** Поставьте ровно ту, что в `.ocamlformat`.

**`dune build -p vibecaml` из папки `vibecaml/` не работает.** Флаг `-p`
фиксирует корень проекта на текущей директории, и dune не видит `dune-project`
уровнем выше. Цели Makefile это уже учитывают.

**`dune test -p vibecaml` падает на `opt-18: not found`.** Флаг `-p` не
фильтрует cram-тесты по пакету, и dune тащит тесты `demo`, которым нужен LLVM.
Поэтому `make test` запускает `dune test` из папки пакета.

**Zanuda молча ничего не находит.** Она читает `.cmt` из `_build` и считает
пути от корня проекта, а не от папки пакета. `make lint` это учитывает и сам
собирает `@check` перед запуском.
