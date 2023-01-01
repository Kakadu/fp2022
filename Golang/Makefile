.PHONY: repl tests test fmt lint celan

all:
	dune build

tests: test
test:
	dune runtest

celan: clean
clean:
	@$(RM) -r _build

fmt:
	dune build @fmt --auto-promote

lint:
	dune build @lint --force

release:
	dune build --profile=release
	dune runtest --profile=release

