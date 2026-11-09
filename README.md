# caprice-lang

This project implements the Caprice programming language.

Caprice is a semantically-typed functional language with types as values.

<img src="./docs/caprice.gif" width="45%" height="45%" />

## Installation

Caprice is built with OCaml 5.5.

Via opam, install OCaml 5.5 and then install the dependencies. Answer y/yes to all questions:

```cmd
opam update
opam switch create 5.5.2
opam install . --deps-only
```

Then, build the repository with dune:

```cmd
dune build
make test
```

The installation is tested on Ubuntu 24.04.5 LTS under WSL2 on Windows 11. There are known issues installing Z3 with earlier versions of gcc, like that which ships with Ubuntu 22.04.

## Developing

Develop in the caprice-lang repository with the following developer tools:

```cmd
opam install ocaml-lsp-server ocamlformat
```

If you are using VS Code, use the OCaml Platform extension.

To write programs in the Caprice language itself, it is suggested to install the VS Code language extension. Navigate to the `caprice-language-extension` directory and follow the instructions in `README.md` there. First start with `caprice-language-extension` to recognize the Caprice language, and then move to `caprice-lsp` to get the language server.

## Benchmarking

For benchmarking, it is recommended to use a compiler switch with flambda for better optimization.

```cmd
opam update
opam switch create 5.5+flambda 5.5.2+options ocaml-option-flambda
opam install . --deps-only
```

Then, build in release mode to perform optimizations and disable expensive assertions.

```cmd
dune build --profile=release
```

Now, run benchmarks on entire folders or specific files. Benchmark throughput with `--sec` or some number of trials with `--trials`. All type checker options still work as command line arguments. E.g.

```cmd
./bench.exe file/path/or/directory --sec 4 -s never -r
```

```cmd
./bench.exe file/path/or/directory --trials 1000 -s only -w no
```

Run `./bench.exe --help` to see the options.

## Programming with Caprice

Write Caprice programs in `.caprice` files. With the project built, you can run the type checker with the `./typecheck.exe` executable. For example,

```cmd
$ ./typecheck.exe ./test/soft-contract/ill-typed/all.caprice
Finished in 0.095 ms and 4 attempted refutations:
    Found error: Refutation: G(poly id : 5, nonce : 33) does not have type bool
    The program is ill typed
```

This means `all.caprice` is an ill-typed program. A polymorphic value was used where a boolean was expected.

All of the possible messages are as follows:
- `Found error`: the type refuter found some type refutation, so **the program is ill-typed**.
- `Exhausted`: every program path was run, and the possible refutation space was exhausted, and **the program is well-typed**.
- `Exhausted pruned tree`: many program paths were run, and the tree was exhausted up to some depth without error, but the program may still be ill-typed because the path tree was pruned.
- `Timeout`: no error was found within the allowed time, so the program may still be ill-typed.
- `Unknown`: the SMT solver failed to return an answer quickly for some query, so some program path was skipped, but no error was found otherwise; the program may still be ill-typed.
