module Goat.Tests.TestHelpers

open Xunit
open Goat.Core.Value

let rec private valEq (a: Value) (b: Value) : bool =
    match a, b with
    | VInt x,     VInt y     -> x = y
    | VFloat x,   VFloat y   -> x = y
    | VBool x,    VBool y    -> x = y
    | VString x,  VString y  -> x = y
    | VUnit,      VUnit      -> true
    | VNil,       VNil       -> true
    | VCons (h1, ThunkRef t1), VCons (h2, ThunkRef t2) ->
        valEq h1 h2 &&
        match !t1, !t2 with
        | Evaluated v1, Evaluated v2 -> valEq v1 v2
        | _ -> true
    | VTuple xs,  VTuple ys  -> List.length xs = List.length ys && List.forall2 valEq xs ys
    | VCtor (n1, fs1), VCtor (n2, fs2) ->
        n1 = n2 && List.length fs1 = List.length fs2 && List.forall2 valEq fs1 fs2
    | VThunk _,   VThunk _   -> true
    | _ -> false

let assertValue (expected: Value) (actual: Value) =
    if not (valEq expected actual) then
        failwith $"Expected:\n  {expected}\nActual:\n  {actual}"

let assertOk (expected: Value) (result: Result<Value, GoatError>) =
    match result with
    | Ok v    -> assertValue expected v
    | Error e -> failwith (sprintf "Expected Ok but got Error: %A" e)

let assertError (result: Result<Value, GoatError>) =
    match result with
    | Error _ -> ()
    | Ok v    -> failwith $"Expected Error but got Ok: {v}"
