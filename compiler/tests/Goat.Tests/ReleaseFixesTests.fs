module Goat.Tests.ReleaseFixesTests

open Xunit
open FsUnit.Xunit
open Goat.Core.Value
open Goat.Core.Parser
open Goat.Core.Desugar
open Goat.Core.Eval
open Goat.Core.Stdlib
open Goat.Tests.TestHelpers

let private run src =
    match fullEnv () with
    | Error e -> failwith (sprintf "%A" e)
    | Ok env  ->
        match parseExpr src with
        | Error m -> failwith $"parse: {m}"
        | Ok expr -> eval env (dsExpr' expr)

let private isOk r = match r with Ok _ -> true | Error _ -> false

[<Fact>]
let ``error raises a UserError`` () =
    match run "error \"boom\"" with
    | Error (UserError "boom") -> ()
    | other -> failwith $"Expected UserError boom, got {other}"

[<Fact>]
let ``error is not silently unit`` () =
    assertError (run "error \"x\"")

[<Fact>]
let ``nested block comments parse`` () =
    let src = "fun main _ =\n  {- outer {- inner -} still outer -}\n  io { do! println \"ok\" }\n"
    parseProgram src "<test>" |> isOk |> should be True

[<Fact>]
let ``single block comment still parses`` () =
    let src = "fun main _ =\n  {- a simple comment -}\n  io { do! println \"ok\" }\n"
    parseProgram src "<test>" |> isOk |> should be True
