# Список целей: make help

PKG = vibecaml
COV_DIR = /tmp/cov-$(PKG)

# Флаг -p фиксирует корень проекта на текущей директории, поэтому цели с ним
# запускаются из корня --- иначе dune не найдёт dune-project уровнем выше.
DUNE_ROOT = cd .. &&

.DEFAULT_GOAL = all
.PHONY: all release test test-runtime check fmt fmt-check lint deps coverage clean help run

all: ## Собрать пакет (профиль dev, быстрая сборка для разработки)
	dune build @all

release: ## Собрать так, как проверяет преподаватель
	$(DUNE_ROOT) dune build -p $(PKG)

# Без -p: флаг не фильтрует cram-тесты по пакету, и dune потащит тесты demo,
# которым нужен LLVM. Запуск из папки ограничивает прогон нашими тестами.
test: ## Прогнать тесты пакета
	dune test

# Те же тесты входят в обычный dune runtest и серверный CI. Здесь только
# удобный отдельный запуск: сборкой, зависимостями и выводом управляет Dune.
test-runtime: ## Принудительно прогнать C-драйверы runtime под QEMU
	dune test runtime_test --force

# Алиас ограничен нашим пакетом: @check из этой папки задевает чужие пакеты
# в общем dune project, и их ошибки роняли make lint.
check: ## Проверить типы без полной сборки, самый быстрый прогон
	$(DUNE_ROOT) dune build @$(PKG)/check

fmt: ## Отформатировать исходники (правит файлы на месте)
	dune build @fmt --auto-promote

fmt-check: ## Проверить форматирование, ничего не правя
	dune build @fmt

# Zanuda в opam-репозитории отсутствует, поэтому цель сначала проверяет, что
# бинарь есть в PATH, и подсказывает команду установки. Проверка присваивания
# отключена только для сгенерированного menhir-кода, см. ТЗ в корневом AGENTS.md.
ZANUDA_PIN = opam pin add zanuda https://github.com/Kakadu/zanuda.git --yes

# Zanuda читает .cmt из _build, поэтому зависит от check и запускается из
# корня проекта --- пути внутри неё считаются от корня, а не от папки пакета.
lint: check ## Прогнать линтер Zanuda
	@command -v zanuda >/dev/null 2>&1 || { \
		echo "make lint: zanuda не найдена в PATH."; \
		echo "  Установить: $(ZANUDA_PIN)"; \
		exit 1; }
	$(DUNE_ROOT) zanuda -dir $(PKG)

# Все opam-файлы курса лежат в корне проекта, а не в папках пакетов, поэтому
# путь до нашего указываем явно --- иначе opam либо не найдёт пакет, либо
# потянет зависимости demo вместе с LLVM.
deps: ## Доставить зависимости пакета из opam
	opam install ../$(PKG).opam --deps-only --with-test --with-doc --yes

coverage: ## Собрать отчёт о покрытии тестами
	$(RM) -r $(COV_DIR)
	mkdir -p $(COV_DIR)
	BISECT_FILE=$(COV_DIR) dune test \
		--instrument-with bisect_ppx_ng --force
	bisect-ppx-report html --coverage-path $(COV_DIR)
	bisect-ppx-report summary --coverage-path $(COV_DIR)
	@echo "Отчёт: xdg-open _coverage/index.html"

clean: ## Удалить артефакты сборки и покрытия
	dune clean
	$(RM) -r $(COV_DIR) _coverage

help: ## Показать этот список
	@grep -hE '^[a-z-]+:.*?## ' $(MAKEFILE_LIST) \
		| awk -F':.*?## ' '{ printf "  \033[1m%-12s\033[0m %s\n", $$1, $$2 }'

# Тулчейн и отладка: docs/riscv.md.
run: ## Собрать PROG=file.s с рантаймом и запустить под QEMU
	scripts/run.sh $(PROG)
