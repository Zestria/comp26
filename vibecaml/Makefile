# Список целей: make help

PKG = vibecaml
COV_DIR = /tmp/cov-$(PKG)

# Флаг -p фиксирует корень проекта на текущей директории, поэтому цели с ним
# запускаются из корня --- иначе dune не найдёт dune-project уровнем выше.
DUNE_ROOT = cd .. &&

.DEFAULT_GOAL = all
.PHONY: all release test check fmt fmt-check lint deps coverage clean help run

all: ## Собрать пакет (профиль dev, быстрая сборка для разработки)
	dune build @all

release: ## Собрать так, как проверяет преподаватель
	$(DUNE_ROOT) dune build -p $(PKG)

# Без -p: флаг не фильтрует cram-тесты по пакету, и dune потащит тесты demo,
# которым нужен LLVM. Запуск из папки ограничивает прогон нашими тестами.
test: ## Прогнать тесты пакета
	dune test

test-runtime1:
	riscv64-linux-gnu-gcc -O3 runtime/runtime.s runtime_test/case1/main.c -nostartfiles -o case1
	-qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./case1 > case1_actual.txt 2>&1
	diff runtime_test/case1/expected.txt case1_actual.txt
	rm case1_actual.txt case1

test-runtime2:
	riscv64-linux-gnu-gcc -g -O3 runtime/runtime.s runtime_test/case2/main.c -nostartfiles -o case2
	-qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./case2 > case2_actual.txt 2>&1
	diff runtime_test/case2/expected.txt case2_actual.txt
	rm case2_actual.txt case2

test-runtime3:
	riscv64-linux-gnu-gcc -O3 -g runtime/runtime.s runtime_test/case3/main.c -nostartfiles -o case3
	-qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./case3 > case3_actual.txt 2>&1
	diff runtime_test/case3/expected.txt case3_actual.txt
	rm case3_actual.txt case3

test-runtime4:
	riscv64-linux-gnu-gcc -O3 runtime/runtime.s runtime_test/case4/main.c -nostartfiles -o case4
	-qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./case4 > case4_actual.txt 2>&1
	diff runtime_test/case4/expected.txt case4_actual.txt
	rm case4_actual.txt case4

test-runtime5:
	riscv64-linux-gnu-gcc -O3 runtime/runtime.s runtime_test/case5/main.c -nostartfiles -o case5
	-qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./case5 > case5_actual.txt 2>&1
	diff runtime_test/case5/expected.txt case5_actual.txt
	rm case5_actual.txt case5

test-runtime: test-runtime1 test-runtime2 test-runtime3 test-runtime4 test-runtime5
	echo "Runtime tests passed!"

check: ## Проверить типы без полной сборки, самый быстрый прогон
	dune build @check

fmt: ## Отформатировать исходники (правит файлы на месте)
	dune build @fmt --auto-promote

fmt-check: ## Проверить форматирование, ничего не правя
	dune build @fmt

# Zanuda в opam-репозитории отсутствует, поэтому цель сначала проверяет, что
# бинарь есть в PATH, и подсказывает команду установки. Проверку присваивания
# в парсерах разрешено отключать через Zanuda, см. ТЗ в корневом AGENTS.md.
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
