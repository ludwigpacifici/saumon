#!/bin/bash

# TODO: When dune 2.0 available, remove this script because of
# https://dune.readthedocs.io/en/latest/formatting.html

cd `git rev-parse --show-toplevel`
find lib test -name '*.ml' -or -name '*.mli' | xargs ocamlformat -i
