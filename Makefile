.RECIPEPREFIX = >
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: test

all:
> dune build @install

run:
> dune exec saumon

test:
> dune runtest

fmt:
> dune build @fmt --auto-promote

install:
> opam install .

uninstall:
> opam uninstall .

clean:
> dune clean
