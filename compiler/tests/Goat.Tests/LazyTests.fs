module Goat.Tests.LazyTests

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

let private runBase src =
    match parseExpr src with
    | Error m -> failwith $"parse: {m}"
    | Ok expr ->
        match eval baseEnv (dsExpr' expr) with
        | Ok v    -> v
        | Error e -> failwith (sprintf "%A" e)

[<Fact>]
let ``lazy creates thunk`` () =
    match runBase "lazy (1 + 2)" with
    | VThunk _ -> ()
    | v        -> failwith $"expected VThunk, got {v}"

[<Fact>]
let ``force evaluates thunk`` () =
    assertValue (VInt 3L) (runBase "force (lazy (1 + 2))")

[<Fact>]
let ``force non-thunk is identity`` () =
    assertValue (VInt 42L) (runBase "force 42")

[<Fact>]
let ``thunk is memoized`` () =
    let counter = ref 0
    let env =
        baseEnv
        |> Map.add "sideEffect"
               (VBuiltin ("sideEffect", fun _ ->
                   counter.Value <- counter.Value + 1
                   Ok (VInt (int64 counter.Value))))
    let src = "let t = lazy (sideEffect ()) in let _ = force t in force t"
    match parseExpr src with
    | Error m -> failwith $"parse: {m}"
    | Ok expr ->
        match eval env (dsExpr' expr) with
        | Ok (VInt n) -> assertValue (VInt 1L) (VInt n)
        | other       -> failwith $"unexpected: {other}"

[<Fact>]
let ``thunk evaluated only once`` () =
    let counter = ref 0
    let env =
        baseEnv
        |> Map.add "tick"
               (VBuiltin ("tick", fun _ ->
                   counter.Value <- counter.Value + 1
                   Ok (VInt (int64 counter.Value))))
    let src = "let t = lazy (tick ()) in let a = force t in let b = force t in a + b"
    match parseExpr src with
    | Error m -> failwith $"parse: {m}"
    | Ok expr ->
        match eval env (dsExpr' expr) with
        | Ok result ->
            assertValue (VInt 2L) result
            Assert.Equal(1, counter.Value)
        | Error e -> failwith (sprintf "%A" e)

[<Fact>]
let ``lazy list head via match`` () =
    assertValue (VInt 1L) (runBase "match 1 :: lazy [] with | h :: _ => h | [] => 0")

[<Fact>]
let ``take from infinite stream head`` () =
    let src = "let rec nats n = n :: lazy (nats (n + 1)) in head (take 3 (nats 0))"
    assertValue (VInt 0L) (runOk src)

[<Fact>]
let ``infinite stream first element`` () =
    let src = "let rec nats n = n :: lazy (nats (n + 1)) in head (nats 42)"
    assertValue (VInt 42L) (runOk src)

[<Fact>]
let ``take 5 from stream length`` () =
    let src = "let rec nats n = n :: lazy (nats (n + 1)) in length (take 5 (nats 0))"
    assertValue (VInt 5L) (runOk src)
