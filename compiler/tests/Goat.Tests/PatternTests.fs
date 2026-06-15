module Goat.Tests.PatternTests

open Xunit
open Goat.Core.Value
open Goat.Core.Builtins
open Goat.Core.Parser
open Goat.Core.Desugar
open Goat.Core.Eval
open Goat.Core.Stdlib
open Goat.Tests.TestHelpers

let private runWith env src =
    match parseExpr src with
    | Error m -> failwith $"parse: {m}"
    | Ok expr -> eval env (dsExpr' expr)

let private runOk src =
    match fullEnv () with
    | Error e -> failwith (sprintf "%A" e)
    | Ok env  ->
        match runWith env src with
        | Ok v    -> v
        | Error e -> failwith (sprintf "%A" e)

[<Fact>]
let ``match int literal`` () =
    assertValue (VString "one") (runOk "match 1 with | 1 => \"one\" | _ => \"other\"")

[<Fact>]
let ``match wildcard`` () =
    assertValue (VString "other") (runOk "match 99 with | 1 => \"one\" | _ => \"other\"")

[<Fact>]
let ``match bool`` () =
    assertValue (VInt 1L) (runOk "match true with | true => 1 | false => 0")

[<Fact>]
let ``match string`` () =
    assertValue (VInt 1L) (runOk "match \"hi\" with | \"hi\" => 1 | _ => 0")

[<Fact>]
let ``match variable binding`` () =
    assertValue (VInt 43L) (runOk "match 42 with | n => n + 1")

[<Fact>]
let ``match empty list`` () =
    assertValue (VInt 0L) (runOk "match [] with | [] => 0 | _ => 1")

[<Fact>]
let ``match cons pattern`` () =
    assertValue (VInt 1L) (runOk "match [1, 2, 3] with | x :: _ => x | [] => 0")

[<Fact>]
let ``match cons head and tail`` () =
    assertValue (VInt 10L) (runOk "match [10, 20] with | h :: t => h | [] => 0")

[<Fact>]
let ``match tuple pattern`` () =
    assertValue (VInt 3L) (runOk "match (1, 2) with | (a, b) => a + b")

[<Fact>]
let ``match guard positive`` () =
    assertValue (VString "pos") (runOk "match 5 with | n when n > 0 => \"pos\" | _ => \"neg\"")

[<Fact>]
let ``match guard negative fallthrough`` () =
    assertValue (VString "neg") (runOk "match -3 with | n when n > 0 => \"pos\" | _ => \"neg\"")

[<Fact>]
let ``match ADT None`` () =
    assertValue (VInt 0L) (runOk "match None with | None => 0 | Some x => x")

[<Fact>]
let ``match ADT Some`` () =
    assertValue (VInt 42L) (runOk "match Some 42 with | None => 0 | Some x => x")

[<Fact>]
let ``match nested tuple`` () =
    assertValue (VInt 6L) (runOk "match ((1, 2), 3) with | ((a, b), c) => a + b + c")

[<Fact>]
let ``match exhaustion fails`` () =
    match fullEnv () with
    | Error e -> failwith (sprintf "%A" e)
    | Ok env  ->
        match runWith env "match 5 with | 1 => \"one\" | 2 => \"two\"" with
        | Error (MatchError _) -> ()
        | other -> failwith $"expected MatchError, got {other}"
