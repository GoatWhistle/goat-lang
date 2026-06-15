module Goat.Tests.EvalTests

open Xunit
open Goat.Core.Value
open Goat.Core.Builtins
open Goat.Core.Parser
open Goat.Core.Desugar
open Goat.Core.Eval
open Goat.Tests.TestHelpers

let private run src =
    match parseExpr src with
    | Error m -> failwith $"parse: {m}"
    | Ok expr -> eval baseEnv (dsExpr' expr)

let private runOk src =
    match run src with
    | Ok v    -> v
    | Error e -> failwith (sprintf "%A" e)

let private runErr src =
    match run src with
    | Error _ -> true
    | Ok _    -> false

[<Fact>]
let ``eval int add`` () = assertValue (VInt 3L) (runOk "1 + 2")

[<Fact>]
let ``eval int sub`` () = assertValue (VInt 6L) (runOk "10 - 4")

[<Fact>]
let ``eval int mul`` () = assertValue (VInt 12L) (runOk "3 * 4")

[<Fact>]
let ``eval int div`` () = assertValue (VInt 5L) (runOk "10 / 2")

[<Fact>]
let ``eval int mod`` () = assertValue (VInt 1L) (runOk "10 mod 3")

[<Fact>]
let ``eval division by zero`` () = Assert.True(runErr "1 / 0")

[<Fact>]
let ``eval float add`` () = assertValue (VFloat 3.0) (runOk "1.0 + 2.0")

[<Fact>]
let ``eval mixed int float`` () = assertValue (VFloat 3.0) (runOk "1 + 2.0")

[<Fact>]
let ``eval equal ints`` () = assertValue (VBool true) (runOk "1 == 1")

[<Fact>]
let ``eval not equal`` () = assertValue (VBool true) (runOk "1 /= 2")

[<Fact>]
let ``eval less than`` () = assertValue (VBool true) (runOk "1 < 2")

[<Fact>]
let ``eval string equality`` () = assertValue (VBool true) (runOk "\"abc\" == \"abc\"")

[<Fact>]
let ``eval string less than`` () = assertValue (VBool true) (runOk "\"apple\" < \"banana\"")

[<Fact>]
let ``eval bool and`` () = assertValue (VBool false) (runOk "true && false")

[<Fact>]
let ``eval bool or`` () = assertValue (VBool true) (runOk "true || false")

[<Fact>]
let ``eval not true`` () = assertValue (VBool false) (runOk "not true")

[<Fact>]
let ``eval if true branch`` () = assertValue (VInt 42L) (runOk "if true then 42 else 0")

[<Fact>]
let ``eval if false branch`` () = assertValue (VInt 0L) (runOk "if false then 42 else 0")

[<Fact>]
let ``eval let binding`` () = assertValue (VInt 10L) (runOk "let x = 5 in x * 2")

[<Fact>]
let ``eval nested let`` () = assertValue (VInt 5L) (runOk "let x = 2 in let y = 3 in x + y")

[<Fact>]
let ``eval let rec factorial`` () =
    assertValue (VInt 120L) (runOk "let rec fact n = if n <= 0 then 1 else n * fact (n - 1) in fact 5")

[<Fact>]
let ``eval lambda application`` () = assertValue (VInt 6L) (runOk "(\\x -> x + 1) 5")

[<Fact>]
let ``eval curried application`` () = assertValue (VInt 7L) (runOk "(\\x y -> x + y) 3 4")

[<Fact>]
let ``eval partial application`` () = assertValue (VInt 10L) (runOk "let add3 = \\x -> x + 3 in add3 7")

[<Fact>]
let ``eval pipe operator`` () = assertValue (VInt 10L) (runOk "5 |> (\\x -> x * 2)")

[<Fact>]
let ``eval tuple`` () =
    assertValue (VTuple [VInt 1L; VInt 2L; VInt 3L]) (runOk "(1, 2, 3)")

[<Fact>]
let ``eval empty list`` () = assertValue VNil (runOk "[]")

[<Fact>]
let ``eval cons head`` () =
    match runOk "1 :: []" with
    | VCons (VInt 1L, _) -> ()
    | v -> failwith $"expected VCons 1, got {v}"

[<Fact>]
let ``eval string concat`` () = assertValue (VString "hello world") (runOk "\"hello\" ++ \" world\"")

[<Fact>]
let ``eval unbound variable errors`` () = Assert.True(runErr "undefinedVar")
