.PHONY: all test lint
all:
	cd .. && dune build -p MyLittleML

test:
	cd .. && dune runtest -p MyLittleML

lint: all
	cd .. && opam exec -- zanuda -dir MyLittleML
