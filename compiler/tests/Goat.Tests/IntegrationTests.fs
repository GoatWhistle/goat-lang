module Goat.Tests.IntegrationTests

open Xunit
open System.IO
open Goat.Core.Value
open Goat.Core.Parser
open Goat.Core.Desugar
open Goat.Core.Eval
open Goat.Core.Stdlib

let private examplesDir =
    let here = __SOURCE_DIRECTORY__
    Path.GetFullPath(Path.Combine(here, "..", "..", "examples"))

let private runProgram src =
    match parseProgram src "<integration>" with
    | Error m -> failwith $"parse: {m}"
    | Ok prog ->
        match fullEnv () with
        | Error e -> failwith (sprintf "%A" e)
        | Ok env  ->
            let desugared = desugar prog
            let sw = new StringWriter()
            let orig = System.Console.Out
            System.Console.SetOut(sw)
            let result =
                try runProgram env desugared
                finally System.Console.SetOut(orig)
            result, sw.ToString()

let private runFile filename =
    let path = Path.Combine(examplesDir, filename)
    if not (File.Exists path) then failwith $"example not found: {path}"
    runProgram (File.ReadAllText path)

let private assertContains (sub: string) (s: string) =
    Assert.True(s.Contains(sub), $"Expected output to contain \"{sub}\"\nActual output:\n{s}")

[<Fact>]
let ``factorial example outputs 120`` () =
    let _, output = runFile "factorial.goat"
    assertContains "120" output

[<Fact>]
let ``factorial example outputs 3628800`` () =
    let _, output = runFile "factorial.goat"
    assertContains "3628800" output

[<Fact>]
let ``fibonacci example runs`` () =
    let result, _ = runFile "fibonacci.goat"
    match result with
    | Ok _ -> ()
    | Error e -> failwith (sprintf "%A" e)

[<Fact>]
let ``list_ops example contains sum`` () =
    let _, output = runFile "list_ops.goat"
    assertContains "55" output

[<Fact>]
let ``option_monad contains Some`` () =
    let _, output = runFile "option_monad.goat"
    assertContains "Some" output

[<Fact>]
let ``option_monad contains None`` () =
    let _, output = runFile "option_monad.goat"
    assertContains "None" output

[<Fact>]
let ``shapes contains area`` () =
    let _, output = runFile "shapes.goat"
    assertContains "area" output

[<Fact>]
let ``lazy_streams contains primes header`` () =
    let _, output = runFile "lazy_streams.goat"
    assertContains "primes" output

[<Fact>]
let ``lazy_streams contains fibonacci header`` () =
    let _, output = runFile "lazy_streams.goat"
    assertContains "fibonacci" output

[<Fact>]
let ``quicksort output contains 11`` () =
    let _, output = runFile "quicksort.goat"
    assertContains "11" output

[<Fact>]
let ``quicksort output contains apple`` () =
    let _, output = runFile "quicksort.goat"
    assertContains "apple" output

[<Fact>]
let ``hello world`` () =
    let _, output = runProgram """fun main _ = io { do! println "Hello, World!"; return () }"""
    Assert.Equal("Hello, World!", output.Trim())

[<Fact>]
let ``recursive fibonacci inline`` () =
    let src = "fun rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)\nfun main _ = io { do! println (show (fib 10)); return () }"
    let _, output = runProgram src
    Assert.Equal("55", output.Trim())

[<Fact>]
let ``factorial via match inline`` () =
    let src = "fun rec fact n = match n with | 0 => 1 | k => k * fact (k - 1)\nfun main _ = io { do! println (show (fact 6)); return () }"
    let result, output = runProgram src
    match result with
    | Error e -> failwith (sprintf "%A" e)
    | Ok _ -> Assert.Equal("720", output.Trim())

[<Fact>]
let ``map filter pipeline inline`` () =
    let src = """
fun isEven x = x mod 2 == 0
fun square x = x * x

fun main _ =
  let xs = filter isEven (map square [1, 2, 3, 4, 5]) in
  io { do! println (show (length xs)); return () }
"""
    let _, output = runProgram src
    Assert.Equal("2", output.Trim())

[<Fact>]
let ``closure captures environment`` () =
    let src = """
fun makeAdder n x = n + x

fun main _ =
  let add5 = makeAdder 5 in
  io { do! println (show (add5 10)); return () }
"""
    let _, output = runProgram src
    Assert.Equal("15", output.Trim())
