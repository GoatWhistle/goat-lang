module Goat.Tests.IOTests

open Xunit
open Goat.Core.Value
open Goat.Core.Parser
open Goat.Core.Desugar
open Goat.Core.Eval
open Goat.Core.Stdlib
open Goat.Tests.TestHelpers

let private runIO src =
    match fullEnv () with
    | Error e -> failwith (sprintf "%A" e)
    | Ok env  ->
        match parseExpr src with
        | Error m -> failwith $"parse: {m}"
        | Ok expr ->
            match eval env (dsExpr' expr) with
            | Error e -> failwith (sprintf "%A" e)
            | Ok (VIO action) ->
                match action () with
                | IOOk v    -> v
                | IOError m -> failwith m
            | Ok v -> failwith $"expected VIO, got {v}"

[<Fact>]
let ``io return int`` () = assertValue (VInt 42L) (runIO "io { return 42 }")

[<Fact>]
let ``io return string`` () = assertValue (VString "hello") (runIO "io { return \"hello\" }")

[<Fact>]
let ``io return unit`` () = assertValue VUnit (runIO "io { return () }")

[<Fact>]
let ``io let! binds result`` () =
    assertValue (VInt 11L) (runIO "io { let! x = io { return 10 }; return (x + 1) }")

[<Fact>]
let ``io let! multiple bindings`` () =
    assertValue (VInt 7L) (runIO "io { let! x = io { return 3 }; let! y = io { return 4 }; return (x + y) }")

[<Fact>]
let ``io let pure binds value`` () =
    assertValue (VInt 10L) (runIO "io { let x = 5; return (x * 2) }")

[<Fact>]
let ``io nested blocks`` () =
    assertValue (VInt 99L) (runIO "io { let! inner = io { return 99 }; return inner }")

[<Fact>]
let ``io block is VIO`` () =
    match fullEnv () with
    | Error e -> failwith (sprintf "%A" e)
    | Ok env  ->
        match parseExpr "io { return 1 }" with
        | Error m -> failwith $"parse: {m}"
        | Ok expr ->
            match eval env (dsExpr' expr) with
            | Ok (VIO _) -> ()
            | other -> failwith $"expected VIO, got {other}"

[<Fact>]
let ``input is VIO`` () =
    match fullEnv () with
    | Error e -> failwith (sprintf "%A" e)
    | Ok env  ->
        match parseExpr "input" with
        | Error m -> failwith $"parse: {m}"
        | Ok expr ->
            match eval env (dsExpr' expr) with
            | Ok (VIO _) -> ()
            | other -> failwith $"expected VIO, got {other}"
