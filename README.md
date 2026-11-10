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
eval $(opam env)
opam install . --deps-only
```

Then, build the repository with dune:

```cmd
dune build
make test
```

`make test` runs the fast tests: the search for an error is expected to terminate, and relatively quickly. Run all tests with `make test-slow`. Some of these tests may fail if the error is not found within the allowed time, specifically two ill-typed interpreter tests (`test/programs/ill-typed/interp/eval_true_body.caprice` and `test/programs/ill-typed/interp/forgot_eval_arg.caprice`).

The installation is tested on Ubuntu 24.04.5 LTS under WSL2 on Windows 11. There are known issues installing Z3 with earlier versions of gcc, like that which ships with Ubuntu 22.04.

## Developing

Develop in the caprice-lang repository with the following developer tools:

```cmd
opam install ocaml-lsp-server ocamlformat
```

If you are using VS Code, use the OCaml Platform extension.

To write programs in the Caprice language itself, it is suggested to install the VS Code language extension. Navigate to the `caprice-language-extension` directory and follow the instructions in `README.md` there. First start with `caprice-language-extension` to recognize the Caprice language, and then move to `caprice-lsp` to get the language server.

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

See the full options by running `./typecheck.exe --help`.

## Benchmarking

For benchmarking, it is recommended to use a compiler switch with flambda for better optimization. This can have massive implications for performance.

```cmd
opam update
opam switch create 5.5+flambda 5.5.2+options ocaml-option-flambda
eval $(opam env)
opam install . --deps-only
```

Then, build in release mode to perform optimizations and disable expensive assertions.

```cmd
dune build --profile=release
```

Now, run benchmarks on entire folders or specific files. Benchmark throughput with `--sec` or some number of trials with `--trials`. All type checker options still work as command line arguments. Run `./bench.exe --help` to see all options.

### Reproducing the paper benchmarks

The benchmarks in the paper are collected by measuring the type-refuter throughput for 10 seconds on each program. However, the results in the paper are presented as average or median time to find an error or to type check the program, or as the peformance ratio between Caprice and [Bluejay](https://github.com/JHU-PL-Lab/jaylang/tree/oopsla-24) (we do not include instructions for measuring Bluejay's performance). The following commands only produce the throughput. For most of these commands, throughput in 1 second provides a good estimate.

#### Figure 11: Caprice vs Bluejay

Throughputs of the programs translated from Bluejay: benchmark the entire `test/bluejay/ill-typed/` directory, with no type splaying, for 10 seconds on each program, and with each new trial randomized.

```cmd
./bench.exe ./test/bluejay/ill-typed/ --sec 10 -s never -r
```

Throughputs of the software contract programs, which are translated from Bluejay:

```cmd
./bench.exe ./test/soft-contract/ill-typed/ --sec 10 -s never -r
```

#### Figure 12: ill-typed Caprice programs

Throughputs of the ill-typed variations of the AVL tree program: benchmark the entire `test/programs/ill-typed/avl/` directory, with no type splaying, for 10 seconds on each program variation, and with each new trial randomized:

```cmd
./bench.exe ./test/programs/ill-typed/avl/ --sec 10 -s never -r
```

The remaining programs are similar, except for `interp`, explained after:

```cmd
./bench.exe ./test/programs/ill-typed/layout/ --sec 10 -s never -r # called layout-tree in the paper
./bench.exe ./test/programs/ill-typed/lex/ --sec 10 -s never -r
./bench.exe ./test/programs/ill-typed/merge-sort/ --sec 10 -s never -r
./bench.exe ./test/programs/ill-typed/minesweeper/ --sec 10 -s never -r
```

The interpreter program has two variations that are slow. Benchmark all but the slow variations:

```cmd
./bench.exe ./test/programs/ill-typed/interp/ --sec 10 -s never -r --sauf './test/programs/ill-typed/interp/forgot_eval_arg.caprice ./test/programs/ill-typed/interp/eval_true_body.caprice'
```

To see that the first can terminate:

```cmd
$ ./typecheck.exe ./test/programs/ill-typed/interp/forgot_eval_arg.caprice -t 600 -s never
Finished in 9035.835 ms and 1664715 attempted refutations:
    Found error: Refutation: (`Minus ((`Int 9), (`Int -8))) does not have type (`Function of { x : (((`Id of int) * (mu t. <body>)) * (mu t. <body>)) | <predicate> } | `Int of int)
    The program is ill typed
```

We benchmarked it in the paper by taking 15 trials (this may take a while):

```cmd
./bench.exe ./test/programs/ill-typed/interp/forgot_eval_arg.caprice --trials 15 -s never -r -t 600
```

The other program, `./test/programs/ill-typed/interp/eval_true_body.caprice`, sometimes terminates, and sometimes runs out of memory when given enough time.

#### Figure 13: type checking well-typed Caprice programs

Throughputs of various well-typed programs for 10 seconds each, with randomness (even though it should have no effect).

```cmd
./bench.exe --sec 10 -r \
    ./test/libraries/ --sauf ./test/libraries/difference_lists.caprice \
    ./test/functors/well-typed/hoist_type_parameter.caprice \
    ./test/functors/well-typed/switch_indexed_monad.caprice \
    ./test/functors/well-typed/writer_transformer.caprice \
    ./test/programs/group.caprice
```
