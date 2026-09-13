.PHONY: copydemo

all:


NEW_NAME = MML
OLD_NAME = demo
copy_template:
	@$(RM) -r $(NEW_NAME)
	cp -r $(OLD_NAME) $(NEW_NAME)
	@$(RM) $(NEW_NAME)/DONT_REMOVE_THIS_DIRECTORY.md
	@sed 's/(name $(OLD_NAME))/(name $(NEW_NAME))/g' $(NEW_NAME)/dune-project -i
	@sed 's/public_name $(OLD_NAME)/public_name $(NEW_NAME)/g' $(NEW_NAME)/bin/dune -i
	@mv $(NEW_NAME)/$(OLD_NAME).opam $(NEW_NAME)/$(NEW_NAME).opam
	@mv $(NEW_NAME)/$(OLD_NAME).opam.template $(NEW_NAME)/$(NEW_NAME).opam.template
	@echo "\033[5m\033[1mПереименуйте Васю Пупкина в себя\033[22m\033[0m"
	grep -n --color=auto -e FIXME -e 'FIXME Vasya Pupkin' $(NEW_NAME)/dune-project -r


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
