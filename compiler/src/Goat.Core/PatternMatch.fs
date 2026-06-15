module Goat.Core.PatternMatch

open Goat.Core.Ast
open Goat.Core.Value

let rec matchPattern (pat: Pattern) (value: Value) : (Ident * Value) list option =
    match pat, value with
    | PWild, _ ->
        Some []

    | PLit (LInt n), VInt m when n = m     -> Some []
    | PLit (LFloat f), VFloat g when f = g -> Some []
    | PLit (LBool b), VBool c when b = c   -> Some []
    | PLit (LString s), VString t when s = t -> Some []
    | PLit LUnit, VUnit                    -> Some []
    | PLit _, _                            -> None

    | PVar name, v ->
        Some [(name, v)]

    | PCons (headPat, tailPat), VCons (head, thunkRef) ->
        let tail =
            let (ThunkRef cell) = thunkRef
            match !cell with
            | Evaluated v -> v
            | _ -> VThunk thunkRef
        match matchPattern headPat head, matchPattern tailPat tail with
        | Some b1, Some b2 -> Some (b1 @ b2)
        | _ -> None

    | PCons (_, _), VNil -> None

    | PList [], VNil -> Some []
    | PList (p :: rest), VCons (head, thunkRef) ->
        let tail =
            let (ThunkRef cell) = thunkRef
            match !cell with
            | Evaluated v -> v
            | _ -> VThunk thunkRef
        match matchPattern p head with
        | None -> None
        | Some b1 ->
            match matchPattern (PList rest) tail with
            | None -> None
            | Some b2 -> Some (b1 @ b2)
    | PList (_ :: _), VNil -> None
    | PList [], _ -> None

    | PTuple pats, VTuple vals when List.length pats = List.length vals ->
        let results = List.map2 matchPattern pats vals
        if List.forall Option.isSome results then
            Some (List.collect Option.get results)
        else
            None
    | PTuple _, _ -> None

    | PCtor (name, pats), VCtor (ctorName, fields)
        when name = ctorName && List.length pats = List.length fields ->
        let results = List.map2 matchPattern pats fields
        if List.forall Option.isSome results then
            Some (List.collect Option.get results)
        else
            None
    | PCtor (name, []), VCtor (ctorName, []) when name = ctorName -> Some []
    | PCtor _, _ -> None

    | PAs (inner, alias), v ->
        matchPattern inner v |> Option.map (fun bs -> (alias, v) :: bs)

    | _ -> None

let tryMatchArms
        (arms: MatchArm list)
        (value: Value)
        (evalGuard: (Ident * Value) list -> Expr -> bool)
        : (MatchArm * (Ident * Value) list) option =
    arms |> List.tryPick (fun arm ->
        match matchPattern arm.Pattern value with
        | None -> None
        | Some bindings ->
            match arm.Guard with
            | None -> Some (arm, bindings)
            | Some guard ->
                if evalGuard bindings guard then Some (arm, bindings)
                else None)
