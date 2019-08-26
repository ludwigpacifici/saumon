# Saumon ><>

Lox language implemented in [OCaml](https://ocaml.org/).

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

Run with Dune:

```bash
$ dune exec saumon
```

Install with Opam:

```bash
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
