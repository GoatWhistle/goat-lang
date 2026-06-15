module Goat.Tests.StdlibTests

open Xunit
open Goat.Core.Value
open Goat.Core.Parser
open Goat.Core.Desugar
open Goat.Core.Eval
open Goat.Core.Stdlib
open Goat.Tests.TestHelpers

let private runOk src =
    match fullEnv () with
    | Error e -> failwith (sprintf "%A" e)
    | Ok env  ->
        match parseExpr src with
        | Error m -> failwith $"parse: {m}"
        | Ok expr ->
            match eval env (dsExpr' expr) with
            | Ok v    -> v
            | Error e -> failwith (sprintf "%A" e)

let private vList vs = List.foldBack (fun v acc -> VCons (v, ThunkRef (ref (Evaluated acc)))) vs VNil
let private vInts ns = vList (List.map (int64 >> VInt) ns)

[<Fact>]
let ``map doubles each element`` () =
    assertValue (vInts [2; 4; 6]) (runOk "map (\\x -> x * 2) [1, 2, 3]")

[<Fact>]
let ``map on empty list`` () =
    assertValue VNil (runOk "map (\\x -> x + 1) []")

[<Fact>]
let ``filter evens`` () =
    assertValue (vInts [2; 4; 6]) (runOk "filter (\\x -> x mod 2 == 0) [1, 2, 3, 4, 5, 6]")

[<Fact>]
let ``filter all false`` () =
    assertValue VNil (runOk "filter (\\x -> x > 100) [1, 2, 3]")

[<Fact>]
let ``fold sum`` () =
    assertValue (VInt 15L) (runOk "fold (\\acc x -> acc + x) 0 [1, 2, 3, 4, 5]")

[<Fact>]
let ``fold string concat`` () =
    assertValue (VString "abc") (runOk "fold (\\acc x -> acc ++ x) \"\" [\"a\", \"b\", \"c\"]")

[<Fact>]
let ``foldr rebuild list`` () =
    assertValue (vInts [1; 2; 3]) (runOk "foldr (\\x acc -> x :: acc) [] [1, 2, 3]")

[<Fact>]
let ``length of list`` () =
    assertValue (VInt 4L) (runOk "length [1, 2, 3, 4]")

[<Fact>]
let ``length of empty list`` () =
    assertValue (VInt 0L) (runOk "length []")

[<Fact>]
let ``reverse list`` () =
    assertValue (vInts [3; 2; 1]) (runOk "reverse [1, 2, 3]")

[<Fact>]
let ``zip two lists length`` () =
    assertValue (VInt 3L) (runOk "length (zip [1, 2, 3] [4, 5, 6])")

[<Fact>]
let ``zip unequal lengths truncates`` () =
    assertValue (VInt 2L) (runOk "length (zip [1, 2, 3] [4, 5])")

[<Fact>]
let ``take 3`` () =
    assertValue (vInts [1; 2; 3]) (runOk "take 3 [1, 2, 3, 4, 5]")

[<Fact>]
let ``take more than length`` () =
    assertValue (vInts [1; 2]) (runOk "take 10 [1, 2]")

[<Fact>]
let ``drop 2`` () =
    assertValue (vInts [3; 4]) (runOk "drop 2 [1, 2, 3, 4]")

[<Fact>]
let ``head of list`` () =
    assertValue (VInt 10L) (runOk "head [10, 20, 30]")

[<Fact>]
let ``tail of list`` () =
    assertValue (vInts [20; 30]) (runOk "tail [10, 20, 30]")

[<Fact>]
let ``append two lists`` () =
    assertValue (vInts [1; 2; 3; 4]) (runOk "append [1, 2] [3, 4]")

[<Fact>]
let ``mapOption Some`` () =
    assertValue (VCtor ("Some", [VInt 6L])) (runOk "mapOption (\\x -> x + 1) (Some 5)")

[<Fact>]
let ``mapOption None`` () =
    assertValue (VCtor ("None", [])) (runOk "mapOption (\\x -> x + 1) None")

[<Fact>]
let ``bindOption Some`` () =
    assertValue (VCtor ("Some", [VInt 6L])) (runOk "bindOption (Some 3) (\\x -> Some (x * 2))")

[<Fact>]
let ``bindOption None`` () =
    assertValue (VCtor ("None", [])) (runOk "bindOption None (\\x -> Some (x * 2))")

[<Fact>]
let ``sort integers`` () =
    assertValue (vInts [1; 2; 3; 5; 8; 9]) (runOk "sort [5, 2, 8, 1, 9, 3]")

[<Fact>]
let ``strLength`` () =
    assertValue (VInt 5L) (runOk "strLength \"hello\"")

[<Fact>]
let ``show int`` () =
    assertValue (VString "42") (runOk "show 42")

[<Fact>]
let ``parseInt string`` () =
    assertValue (VCtor ("Some", [VInt 123L])) (runOk "parseInt \"123\"")

[<Fact>]
let ``parseInt invalid`` () =
    assertValue (VCtor ("None", [])) (runOk "parseInt \"abc\"")
