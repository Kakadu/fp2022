.PHONY: test check
	
build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop lib

sample_file:
	OCAMLRUNPARAM=b dune exec csv_files/csv1.exe

file:
	OCAMLRUNPARAM=b dune exec test/table_example.exe

start:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f camel_db.zip
	zip -r camel_db.zip . -x@exclude.lst

docs:
	dune build @doc

#########

all:
	dune build

repl:
	OCAMLRUNPARAM=b dune exec ./REPL.exe

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
