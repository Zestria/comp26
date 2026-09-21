.PHONY: all test fmt copy_template

all:
	dune build

test:
	dune test

fmt:
	dune build @fmt --auto-promote

# Скопировать demo/ как заготовку нового компилятора: make copy_template NEW_NAME=foo
# Все пакеты живут в одном dune project, поэтому dune-project и *.opam лежат в
# корне и копированию не подлежат --- пакет дописывается в корневой dune-project
# руками.
NEW_NAME ?= demo2
OLD_NAME = demo
copy_template:
	@$(RM) -r $(NEW_NAME)
	cp $(OLD_NAME) $(NEW_NAME) -r
	@$(RM) -r $(NEW_NAME)/_build $(NEW_NAME)/_coverage
	@sed 's/(package $(OLD_NAME))/(package $(NEW_NAME))/g' $(NEW_NAME)/dune -i
	@sed 's/(public_name $(OLD_NAME)\./(public_name $(NEW_NAME)./g' $(NEW_NAME)/bin/dune -i
	@printf '\033[1mДальше руками: добавьте пакет %s в корневой dune-project и впишитесь в teams.yml\033[0m\n' '$(NEW_NAME)'
	@grep -rn --color=auto -e FIXME -e 'Vasya Pupkin' $(NEW_NAME) || true


TEST_COV_D = /tmp/cov
COVERAGE_OPTS = --coverage-path $(TEST_COV_D) #--expect bin/

.PHONY: test_coverage coverage
test_coverage: coverage
coverage:
	$(RM) -r $(TEST_COV_D)
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)  dune runtest --no-print-directory \
		--instrument-with bisect_ppx_ng --force
	bisect-ppx-report html $(COVERAGE_OPTS)
	bisect-ppx-report summary $(COVERAGE_OPTS)
	@echo "Use 'xdg-open _coverage/index.html' to see coverage report"
