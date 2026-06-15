# Goat — compiler

The source of the functional language **Goat** in F#: lexer, parser (FParsec), desugaring, tree-walking interpreter, standard library, REPL, and CLI.

For the full feature, syntax, and standard-library reference, see the [root README](../README.md) and the [documentation site](https://goatwhistle.github.io/goat-lang/).

## Requirements

- .NET SDK 10

## Build and test

```powershell
dotnet restore Goat.slnx
dotnet build Goat.slnx
dotnet test Goat.slnx
```

## Run

```powershell
dotnet run --project src/Goat.Cli -- run examples/factorial.goat   # run a program
dotnet run --project src/Goat.Cli -- check examples/factorial.goat # check syntax only
dotnet run --project src/Goat.Repl                                 # REPL
```

With the Makefile:

```bash
make build
make test
make repl
make run FILE=examples/fibonacci.goat
make examples
```

## Layout

```text
src/Goat.Core      AST, parser (FParsec), desugaring, interpreter, builtins, stdlib
src/Goat.Cli       CLI: run / check / repl
src/Goat.Repl      interactive REPL with :help/:load/:env/:reset
tests/Goat.Tests   142 xUnit tests (parser, eval, pattern, lazy, IO, stdlib, integration)
examples/          example programs in Goat
```

## Coursework checklist

- [x] **Named bindings** — `let x = expr in body`
- [x] **Recursion** — `fun rec f x = ... f ...`
- [x] **Lazy evaluation** — `lazy expr`, `force thunk`, lazy list tails, memoization
- [x] **Functions** — first class, currying, partial application
- [x] **Closures** — `VClosure` captures the environment
- [x] **File IO library functions** — `readFile`, `writeFile`, `appendFile`
- [x] **Lists / sequences** — cons lists, lazy streams, ranges
- [x] **List library functions** — `map`, `filter`, `fold`, `zip`, `sort`, and more

### Additional features

- [x] **Pattern matching** with guards and nested patterns
- [x] **Algebraic data types** (`type`, constructors, ADT patterns)
- [x] **IO monad** (`io {}`, `do!`, `let!`, `return`)
- [x] **Infinite coinductive structures** (the sieve of Eratosthenes via lazy lists)
- [x] **142 tests** covering the parser, interpreter, patterns, laziness, IO, stdlib, and integration

## Use of generative AI

The project was written by the team by hand. Generative AI was used only to prepare the documentation and the GitHub Pages deployment:

- **Claude Code** generated the structure of the multi-page documentation site from the finished interpreter. Prompt: "from this functional interpreter, create a complete reference with syntax, the standard library, examples, and a description of the internals (lexer, parser, evaluator, IO monad, lazy evaluation). The site should be navigable with a sidebar, work without JavaScript, and be responsive."
- **OpenAI Codex** generated annotated versions of the example programs for the documentation. Prompt: "for this functional language, write example programs (factorial, Fibonacci, quicksort, sieve of Eratosthenes, pattern matching, IO) with explanations of the key constructs and expected output."
- **Claude Code** wrote the README from the minimal README in the first commit and the structure of the finished interpreter.
