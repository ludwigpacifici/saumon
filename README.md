# Saumon ><>

[Lox](http://www.craftinginterpreters.com/the-lox-language.html) language implemented in [OCaml](https://ocaml.org/).

## Goals

* Learn about compiler front ends and interpreters.
* Play and learn with OCaml and its ecosystem. Note, this implementation diverge slightly from the book ; OCaml idiomatic code is quite different from Java (used for the original implementation).

## Build

With [Dune](https://dune.build/):

```bash
$ dune build @install
$ dune exec saumon
```

## Tests

```bash
$ dune runtest
```

## Usage

To use saumon in interactive mode, simply run

```bash
$ saumon
```

Install with Opam:

```bash
$ # cd project/root/folder
$ opam install .
```

## Continuous Integration [![Build Status](https://travis-ci.org/ludwigpacifici/saumon.svg?branch=master)](https://travis-ci.org/ludwigpacifici/saumon)

Travis CI: https://travis-ci.org/ludwigpacifici/saumon

## Progress

* A Tree-Walk Interpreter: https://github.com/ludwigpacifici/saumon/projects/1
* A Bytecode Virtual Machine: https://github.com/ludwigpacifici/saumon/projects/2

## Acknowledgments

1. Designed by Bob Nystrom, [A handbook for making programming languages](http://www.craftinginterpreters.com/).
2. Inspired by [Rulox](https://github.com/mariosangiorgio/rulox).

## License

See [LICENSE](LICENSE).
