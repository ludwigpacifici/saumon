#!/bin/bash

cd `git rev-parse --show-toplevel`

find lib test -name '*.ml' -or -name '*.mli' | xargs ocamlformat -i
