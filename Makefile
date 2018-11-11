.PHONY: help all clean lib test main utop

help:
	@echo "help        Help"
	@echo "all         Run: clean build test"
	@echo "clean       Clean the build"
	@echo "lib         Build the library"
	@echo "test        Build & run the tests"
	@echo "main        Build & run the main tool"
	@echo "utop        Build the library & launch utop"

all: clean build test

clean:
	dune clean

lib:
	dune build lib/rlp.a

test:
	dune exec test/test.exe

main:
	dune exec bin/main.exe

utop:
	dune utop lib
