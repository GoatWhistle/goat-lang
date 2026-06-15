<div align="center">

# Goat

**A functional programming language in F#** — lexer, parser, tree-walking interpreter, REPL, and CLI, together with a full documentation site.

[**Documentation**](https://goatwhistle.github.io/goat-lang/docs/) · [Language](https://goatwhistle.github.io/goat-lang/docs/comments-literals/) · [Standard library](https://goatwhistle.github.io/goat-lang/docs/stdlib/types/) · [Examples](https://goatwhistle.github.io/goat-lang/docs/examples/)

</div>

---

## Repository layout

| Directory | Contents |
| --- | --- |
| [`compiler/`](compiler/) | The language source: interpreter (F#), standard library, REPL, CLI, tests, and example programs |
| [`site/`](site) | The documentation site (static HTML/CSS), published to GitHub Pages |

```text
compiler/
  src/Goat.Core      AST, parser (FParsec), desugaring, interpreter, builtins, stdlib
  src/Goat.Cli       CLI: run / check / repl
  src/Goat.Repl      interactive REPL with :help/:load/:env/:reset
  tests/Goat.Tests   142 xUnit tests (parser, eval, pattern, lazy, IO, stdlib, integration)
  examples/          example programs in Goat
site/
  index.html         overview, quick start, navigation
  assets/            logo, favicons
  docs/              multi-page reference (language, stdlib, examples)
```

## Language features

| Feature | Syntax |
| --- | --- |
| Named bindings | `let x = 42 in x * 2` |
| Functions | `fun double x = x * 2` |
| Recursion | `fun rec fact n = if n <= 0 then 1 else n * fact (n-1)` |
| Lambdas | `\x -> x + 1` |
| Partial application | `let add3 = add 3` |
| Closures | `fun makeAdder n x = n + x` |
| Pattern matching | `match xs with \| [] => 0 \| x :: rest => x` |
| Guarded patterns | `match n with \| x when x > 0 => "pos" \| _ => "neg"` |
| Algebraic data types | `type Option = None \| Some of x` |
| Lazy evaluation | `lazy (expensive ())`, `force thunk` |
| Infinite streams | `let rec nats n = n :: lazy (nats (n+1))` |
| IO monad | `io { let! x = readFile "f"; do! println x; return () }` |
| Lists | `[1, 2, 3]`, `1 :: rest`, `[1..10]` |
| Tuples | `(1, "hello", true)` |
| Pipe operator | `xs \|> map f \|> filter g` |

## Standard library

**Lists:** `map`, `filter`, `fold`, `foldr`, `length`, `reverse`, `zip`, `take`, `drop`, `head`, `tail`, `append`, `sort`

**Option:** `mapOption`, `bindOption`, `fromOption`, `isNone`, `isSome`

**Strings:** `strLength`, `chars`, `words`, `lines`, `trim`, `toUpper`, `toLower`, `contains`, `replace`, `show`, `parseInt`, `parseFloat`

**IO:** `println`, `print`, `input`, `readFile`, `writeFile`, `appendFile`, `fileExists`

**Math:** `abs`, `max`, `min`, `sqrt`, `floor`, `ceil`, `toFloat`

## Requirements

- .NET SDK 10

## Quick start

```powershell
cd compiler
dotnet restore Goat.slnx
dotnet build Goat.slnx
dotnet test Goat.slnx
```

Run a program:

```powershell
cd compiler
dotnet run --project src/Goat.Cli -- run examples/factorial.goat
```

Check syntax:

```powershell
cd compiler
dotnet run --project src/Goat.Cli -- check examples/factorial.goat
```

REPL:

```powershell
cd compiler
dotnet run --project src/Goat.Repl
```

With the Makefile (from the `compiler/` directory):

```bash
make build      # compile
make test       # 142 xUnit tests
make repl       # start the REPL
make run FILE=examples/fibonacci.goat
make examples   # run every example
```

## Examples

### Factorial

```goat
fun rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

fun main _ =
  io {
    do! println (show (factorial 10))
  }
```

### Pattern matching with ADTs

```goat
type Shape = Circle of r | Rect of w h

fun area shape =
  match shape with
  | Circle r => 3.14159 * r * r
  | Rect w h => w * h

fun main _ =
  io {
    do! println (show (area (Circle 5.0)))
    do! println (show (area (Rect 4.0 6.0)))
  }
```

### Infinite lazy streams

```goat
fun rec nats n = n :: lazy (nats (n + 1))

fun main _ =
  let first10 = take 10 (nats 0) in
  io { do! println (show first10) }
```

### The IO monad

```goat
fun main _ =
  io {
    let! content = readFile "data.txt"
    let lineCount = length (lines content)
    do! println ("Lines: " ++ show lineCount)
    return ()
  }
```

## Example programs

| File | Demonstrates |
| --- | --- |
| `factorial.goat` | recursion, pattern matching |
| `fibonacci.goat` | tail recursion, infinite streams |
| `list_ops.goat` | map/filter/fold/zip/sort, ranges |
| `lazy_streams.goat` | sieve of Eratosthenes, infinite sequences |
| `option_monad.goat` | Option ADT, combinators |
| `quicksort.goat` | quicksort + mergesort, string sorting |
| `shapes.goat` | algebraic data types, polymorphism |
| `file_io.goat` | the IO monad, working with files |

## Authors

| Contributor | Role |
| --- | --- |
| [Mikhail Khorokhorin](https://github.com/mikhailkhorokhorin) | Interpreter architecture, evaluator, pattern matching, IO system, documentation |
| [Ivan Gerasimov](https://github.com/ivanGMAI) | Lexer, parser (FParsec), AST, desugar pass, standard library, example programs |
| [Timofei Pupykin](https://github.com/timofeipupykin) | Builtins, REPL, CLI, tests (xUnit), lazy evaluation, CI/CD |
