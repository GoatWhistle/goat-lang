module Goat.Tests.ParserTests

open Xunit
open FsUnit.Xunit
open Goat.Core.Ast
open Goat.Core.Parser

let private parseExprOk src =
    match parseExpr src with
    | Ok e   -> e
    | Error m -> failwith m

let private parseProgramOk src =
    match parseProgram src "<test>" with
    | Ok p   -> p
    | Error m -> failwith m

let private isOk r = match r with Ok _ -> true | Error _ -> false
let private isErr r = not (isOk r)

[<Fact>]
let ``parse int literal`` () =
    parseExprOk "42" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse negative int`` () =
    parseExprOk "-7" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse float literal`` () =
    parseExprOk "3.14" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse bool true`` () =
    parseExprOk "true" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse bool false`` () =
    parseExprOk "false" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse unit`` () =
    parseExprOk "()" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse string literal`` () =
    parseExprOk "\"hello\"" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse string with escapes`` () =
    parseExprOk "\"line1\\nline2\"" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse addition`` () =
    parseExprOk "1 + 2" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse nested arithmetic`` () =
    parseExprOk "1 + 2 * 3 - 4" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse mod operator`` () =
    parseExprOk "10 mod 3" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse not-equal operator`` () =
    parseExprOk "x /= y" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse let in`` () =
    parseExprOk "let x = 1 in x" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse let rec`` () =
    parseExprOk "let rec f x = f x in f 0" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse lambda`` () =
    parseExprOk "\\x -> x + 1" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse lambda multi param`` () =
    parseExprOk "\\x y -> x + y" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse if then else`` () =
    parseExprOk "if true then 1 else 2" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse empty list`` () =
    parseExprOk "[]" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse list literal`` () =
    parseExprOk "[1, 2, 3]" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse cons expression`` () =
    parseExprOk "1 :: [2, 3]" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse tuple`` () =
    parseExprOk "(1, 2)" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse pipe operator`` () =
    parseExprOk "x |> f" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse lazy`` () =
    parseExprOk "lazy (1 + 2)" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse force`` () =
    parseExprOk "force x" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse match`` () =
    parseExprOk "match x with | 0 => 1 | _ => 2" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse match with guard`` () =
    parseExprOk "match x with | n when n > 0 => n | _ => 0" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse io return`` () =
    parseExprOk "io { return () }" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse io with let pure`` () =
    parseExprOk "io { let x = 1; return x }" |> should be (instanceOfType<Expr>)

[<Fact>]
let ``parse fun declaration`` () =
    parseProgramOk "fun double x = x * 2" |> should not' (be Empty)

[<Fact>]
let ``parse type declaration`` () =
    parseProgramOk "type Color = Red | Green | Blue" |> should not' (be Empty)

[<Fact>]
let ``parse multiple declarations`` () =
    parseProgramOk "fun id x = x\nfun double x = x * 2" |> should not' (be Empty)

[<Fact>]
let ``parse error on invalid syntax`` () =
    parseExpr "let = 5"
    |> isErr
    |> should equal true
