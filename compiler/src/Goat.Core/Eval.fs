module Goat.Core.Eval

open Goat.Core.Ast
open Goat.Core.Value
open Goat.Core.Environment
open Goat.Core.PatternMatch

type ResultBuilder() =
    member _.Return x       = Ok x
    member _.ReturnFrom x   = x
    member _.Bind (m, f)    = Result.bind f m
    member _.Zero ()        = Ok VUnit

let result = ResultBuilder()

let rec evalBinOp (op: BinOp) (l: Value) (r: Value) (sp: Span) : EvalResult =
    match op, l, r with
    | Add, VInt a,    VInt b    -> Ok (VInt (a + b))
    | Add, VFloat a,  VFloat b  -> Ok (VFloat (a + b))
    | Add, VInt a,    VFloat b  -> Ok (VFloat (float a + b))
    | Add, VFloat a,  VInt b    -> Ok (VFloat (a + float b))
    | Sub, VInt a,    VInt b    -> Ok (VInt (a - b))
    | Sub, VFloat a,  VFloat b  -> Ok (VFloat (a - b))
    | Sub, VInt a,    VFloat b  -> Ok (VFloat (float a - b))
    | Sub, VFloat a,  VInt b    -> Ok (VFloat (a - float b))
    | Mul, VInt a,    VInt b    -> Ok (VInt (a * b))
    | Mul, VFloat a,  VFloat b  -> Ok (VFloat (a * b))
    | Mul, VInt a,    VFloat b  -> Ok (VFloat (float a * b))
    | Mul, VFloat a,  VInt b    -> Ok (VFloat (a * float b))
    | Div, VInt _,    VInt 0L   -> Error (DivisionByZero (Some sp))
    | Div, VInt a,    VInt b    -> Ok (VInt (a / b))
    | Div, VFloat a,  VFloat b  -> Ok (VFloat (a / b))
    | Div, VInt a,    VFloat b  -> Ok (VFloat (float a / b))
    | Div, VFloat a,  VInt b    -> Ok (VFloat (a / float b))
    | Mod, VInt _,    VInt 0L   -> Error (DivisionByZero (Some sp))
    | Mod, VInt a,    VInt b    -> Ok (VInt (a % b))
    | Pow, VInt a,    VInt b    -> Ok (VFloat (System.Math.Pow(float a, float b)))
    | Pow, VFloat a,  VFloat b  -> Ok (VFloat (System.Math.Pow(a, b)))
    | Pow, VInt a,    VFloat b  -> Ok (VFloat (System.Math.Pow(float a, b)))
    | Pow, VFloat a,  VInt b    -> Ok (VFloat (System.Math.Pow(a, float b)))
    | Eq,  VInt a,    VInt b    -> Ok (VBool (a = b))
    | Eq,  VFloat a,  VFloat b  -> Ok (VBool (a = b))
    | Eq,  VBool a,   VBool b   -> Ok (VBool (a = b))
    | Eq,  VString a, VString b -> Ok (VBool (a = b))
    | Eq,  VUnit,     VUnit     -> Ok (VBool true)
    | Eq,  VNil,      VNil      -> Ok (VBool true)
    | Eq,  _,         _         -> Ok (VBool false)
    | NEq, VInt a,    VInt b    -> Ok (VBool (a <> b))
    | NEq, VFloat a,  VFloat b  -> Ok (VBool (a <> b))
    | NEq, VBool a,   VBool b   -> Ok (VBool (a <> b))
    | NEq, VString a, VString b -> Ok (VBool (a <> b))
    | NEq, VUnit,     VUnit     -> Ok (VBool false)
    | NEq, VNil,      VNil      -> Ok (VBool false)
    | NEq, _,         _         -> Ok (VBool true)
    | Lt,  VInt a,   VInt b    -> Ok (VBool (a < b))
    | Lt,  VFloat a, VFloat b  -> Ok (VBool (a < b))
    | Lt,  VString a, VString b -> Ok (VBool (a < b))
    | Gt,  VInt a,   VInt b    -> Ok (VBool (a > b))
    | Gt,  VFloat a, VFloat b  -> Ok (VBool (a > b))
    | Gt,  VString a, VString b -> Ok (VBool (a > b))
    | LEq, VInt a,   VInt b    -> Ok (VBool (a <= b))
    | LEq, VFloat a, VFloat b  -> Ok (VBool (a <= b))
    | LEq, VString a, VString b -> Ok (VBool (a <= b))
    | GEq, VInt a,   VInt b    -> Ok (VBool (a >= b))
    | GEq, VFloat a, VFloat b  -> Ok (VBool (a >= b))
    | GEq, VString a, VString b -> Ok (VBool (a >= b))
    | And, VBool a,  VBool b   -> Ok (VBool (a && b))
    | Or,  VBool a,  VBool b   -> Ok (VBool (a || b))
    | Append, VString a, VString b -> Ok (VString (a + b))
    | Append, VNil, vs -> Ok vs
    | Append, vs, VNil -> Ok vs
    | Append, VCons _, _ ->
        let rec appendList a b =
            match a with
            | VNil -> Ok b
            | VCons (h, tRef) ->
                result {
                    let! t = forceThunk tRef
                    let! rest = appendList t b
                    return VCons (h, ThunkRef (ref (Evaluated rest)))
                }
            | _ -> Error (TypeError ("++ expects lists", Some sp))
        appendList l r
    | _ ->
        Error (TypeError (
            $"Operator {op} not applicable to {typeNameOf l} and {typeNameOf r}", Some sp))

and forceThunk (ThunkRef cell : ThunkRef) : EvalResult =
    match !cell with
    | Evaluated v -> Ok v
    | BlackHole   -> Error StackOverflow_
    | Unevaluated (env, expr) ->
        cell := BlackHole
        match eval env expr with
        | Ok v    -> cell := Evaluated v; Ok v
        | Error e -> Error e

and bindParam (p: Param) (v: Value) (env: Env) : Env =
    match p with
    | PSimple name -> extend name v env
    | PAnn (name, _) -> extend name v env
    | PWild_ -> env

and applyValue (fn: Value) (arg: Value) (sp: Span) : EvalResult =
    match fn with
    | VThunk tref ->
        match forceThunk tref with
        | Error e -> Error e
        | Ok v    -> applyValue v arg sp
    | VClosure (closEnv, [p], body) ->
        let env' = bindParam p arg closEnv
        eval env' body
    | VClosure (closEnv, p :: rest, body) ->
        let env' = bindParam p arg closEnv
        Ok (VClosure (env', rest, body))
    | VClosure (_, [], body) ->
        Ok fn
    | VBuiltin (_, f) ->
        f arg
    | VCtor (name, fields) ->
        Ok (VCtor (name, fields @ [arg]))
    | _ ->
        Error (TypeError ($"Cannot apply {typeNameOf fn} as a function", Some sp))

and eval (env: Env) (expr: Expr) : EvalResult =
    match expr with
    | Lit (LInt n,    _) -> Ok (VInt n)
    | Lit (LFloat f,  _) -> Ok (VFloat f)
    | Lit (LBool b,   _) -> Ok (VBool b)
    | Lit (LString s, _) -> Ok (VString s)
    | Lit (LUnit,     _) -> Ok VUnit

    | Var (name, sp) ->
        lookup name (Some sp) env

    | Ann (e, _, _) ->
        eval env e

    | Lam (parms, body, _) ->
        Ok (VClosure (env, parms, body))

    | App (fn, arg, sp) ->
        result {
            let! fnVal  = eval env fn
            let! argVal = eval env arg
            return! applyValue fnVal argVal sp
        }

    | BinOp (Pipe, l, r, sp) ->
        result {
            let! lv = eval env l
            let! rv = eval env r
            return! applyValue rv lv sp
        }

    | BinOp (LazyPipe, l, r, sp) ->
        result {
            let thunk = makeThunk env l
            let! rv   = eval env r
            return! applyValue rv thunk sp
        }

    | BinOp (op, l, r, sp) ->
        result {
            let! lv = eval env l
            let! rv = eval env r
            return! evalBinOp op lv rv sp
        }

    | UnOp (Neg, e, sp) ->
        result {
            let! v = eval env e
            return!
                match v with
                | VInt n   -> Ok (VInt (-n))
                | VFloat f -> Ok (VFloat (-f))
                | _ -> Error (TypeError ($"Cannot negate {typeNameOf v}", Some sp))
        }

    | UnOp (Not, e, sp) ->
        result {
            let! v = eval env e
            return!
                match v with
                | VBool b -> Ok (VBool (not b))
                | _ -> Error (TypeError ($"Cannot 'not' {typeNameOf v}", Some sp))
        }

    | If (cond, thenE, elseE, sp) ->
        result {
            let! cv = eval env cond
            return!
                match cv with
                | VBool true  -> eval env thenE
                | VBool false -> eval env elseE
                | _ -> Error (TypeError ("if condition must be Bool", Some sp))
        }

    | Let (isRec, name, [], body, cont, _) ->
        result {
            let! v =
                if isRec then

                    let cell = ref (Unevaluated (env, body))
                    let thunkRef = ThunkRef cell
                    let env' = extend name (VThunk thunkRef) env
                    match eval env' body with
                    | Ok closure ->
                        cell := Evaluated closure
                        Ok closure
                    | Error e -> Error e
                else
                    eval env body
            return! eval (extend name v env) cont
        }

    | Let (_, _, _ :: _, _, _, sp) ->
        Error (TypeError ("Let with params should be desugared first", Some sp))

    | Match (scrut, arms, sp) ->
        result {
            let! raw = eval env scrut
            let! v =
                match raw with
                | VThunk tref -> forceThunk tref
                | other -> Ok other
            let guardEval bindings guardExpr =
                let env' = extendMany bindings env
                match eval env' guardExpr with
                | Ok (VBool b) -> b
                | _ -> false
            return!
                match tryMatchArms arms v guardEval with
                | None ->
                    Error (MatchError ($"Non-exhaustive patterns matching {typeNameOf v}", Some sp))
                | Some (arm, bindings) ->
                    let env' = extendMany bindings env
                    eval env' arm.Body
        }

    | ListLit (elems, _) ->
        let rec buildList acc = function
            | [] -> Ok (List.foldBack (fun v tl -> VCons (v, ThunkRef (ref (Evaluated tl)))) acc VNil)
            | e :: rest ->
                match eval env e with
                | Error err -> Error err
                | Ok v -> buildList (acc @ [v]) rest
        buildList [] elems

    | Cons (h, t, _) ->
        result {
            let! hv = eval env h
            let thunkRef = ThunkRef (ref (Unevaluated (env, t)))
            return VCons (hv, thunkRef)
        }

    | Tuple (elems, _) ->
        let rec buildTuple acc = function
            | [] -> Ok (VTuple (List.rev acc))
            | e :: rest ->
                match eval env e with
                | Error err -> Error err
                | Ok (VThunk tref) ->
                    match forceThunk tref with
                    | Error err -> Error err
                    | Ok v -> buildTuple (v :: acc) rest
                | Ok v -> buildTuple (v :: acc) rest
        buildTuple [] elems

    | Lazy (inner, _) ->
        Ok (makeThunk env inner)

    | Force (inner, sp) ->
        result {
            let! v = eval env inner
            return!
                match v with
                | VThunk ref -> forceThunk ref
                | other      -> Ok other
        }

    | Range (lo, hi, sp) ->
        result {
            let! lv = eval env lo
            let! hv = eval env hi
            return!
                match lv, hv with
                | VInt a, VInt b ->
                    let lst = [ a .. b ] |> List.map VInt
                    Ok (List.foldBack (fun v tl -> VCons (v, ThunkRef (ref (Evaluated tl)))) lst VNil)
                | _ -> Error (TypeError ("Range requires Int boundaries", Some sp))
        }

    | Ctor (name, args, _) ->
        let rec evalArgs acc = function
            | [] -> Ok (VCtor (name, List.rev acc))
            | a :: rest ->
                match eval env a with
                | Error e -> Error e
                | Ok v    -> evalArgs (v :: acc) rest
        evalArgs [] args

    | IOBlock (stmts, _) ->
        Ok (VIO (fun () -> runIOStmts env stmts))

and runIOStmts (env: Env) (stmts: IOStmt list) : IOResult =
    match stmts with
    | [] -> IOOk VUnit
    | IOReturn e :: _ ->
        match eval env e with
        | Ok v    -> IOOk v
        | Error e -> IOError (formatError e)
    | IODo e :: rest ->
        match eval env e with
        | Ok (VIO action) ->
            match action () with
            | IOOk _    -> runIOStmts env rest
            | IOError m -> IOError m
        | Ok _ -> IOError "do! expects an IO action"
        | Error e -> IOError (formatError e)
    | IOLet (name, e) :: rest ->
        match eval env e with
        | Ok (VIO action) ->
            match action () with
            | IOOk v    -> runIOStmts (extend name v env) rest
            | IOError m -> IOError m
        | Ok _ -> IOError "let! expects an IO action"
        | Error e -> IOError (formatError e)
    | IOLetPure (name, e) :: rest ->
        match eval env e with
        | Ok v    -> runIOStmts (extend name v env) rest
        | Error e -> IOError (formatError e)

and formatError (e: GoatError) : string =
    match e with
    | TypeError (msg, Some sp)      -> $"TypeError at {sp.Start.Line}:{sp.Start.Column}: {msg}"
    | TypeError (msg, None)         -> $"TypeError: {msg}"
    | NameError (msg, Some sp)      -> $"NameError at {sp.Start.Line}:{sp.Start.Column}: {msg}"
    | NameError (msg, None)         -> $"NameError: {msg}"
    | MatchError (msg, Some sp)     -> $"MatchError at {sp.Start.Line}:{sp.Start.Column}: {msg}"
    | MatchError (msg, None)        -> $"MatchError: {msg}"
    | DivisionByZero (Some sp)      -> $"DivisionByZero at {sp.Start.Line}:{sp.Start.Column}"
    | DivisionByZero None           -> "DivisionByZero"
    | IOErr msg                     -> $"IOError: {msg}"
    | StackOverflow_                -> "StackOverflow: infinite recursion or forced cycle"
    | UserError msg                 -> $"Error: {msg}"
    | ArityError (msg, Some sp)     -> $"ArityError at {sp.Start.Line}:{sp.Start.Column}: {msg}"
    | ArityError (msg, None)        -> $"ArityError: {msg}"

let rec showValue (v: Value) : string =
    let rec showNested value =
        match value with
        | VString s -> "\"" + s + "\""
        | other -> showValue other

    match v with
    | VInt n    -> string n
    | VFloat f  ->
        let s = sprintf "%g" f
        if s.Contains('.') || s.Contains('e') then s else s + ".0"
    | VBool b   -> if b then "true" else "false"
    | VString s -> s
    | VUnit     -> "()"
    | VNil      -> "[]"
    | VCons _   ->
        let rec collect remaining acc v =
            if remaining = 0 then
                List.rev ("..." :: acc)
            else
                match v with
                | VNil -> List.rev acc
                | VCons (h, tailRef) ->
                    let tail =
                        match forceThunk tailRef with
                        | Ok t -> t
                        | Error _ -> VString "..."
                    collect (remaining - 1) (showNested h :: acc) tail
                | VThunk tref ->
                    match forceThunk tref with
                    | Ok forced -> collect remaining acc forced
                    | Error _ -> List.rev ("..." :: acc)
                | other -> List.rev (showNested other :: acc)
        "[" + String.concat ", " (collect 1000 [] v) + "]"
    | VTuple vs -> "(" + String.concat ", " (List.map showNested vs) + ")"
    | VClosure _ -> "<function>"
    | VBuiltin (name, _) -> $"<builtin:{name}>"
    | VCtor (name, []) -> name
    | VCtor (name, vs) -> name + " " + String.concat " " (List.map showValue vs)
    | VThunk _ -> "<thunk>"
    | VIO _    -> "<IO>"

let evalDecl (env: Env) (decl: Decl) : Result<Env, GoatError> =
    match decl with
    | DLet (isRec, name, [], body, _) ->
        let evalEnv = if isRec then
                          let cell = ref (Unevaluated (env, body))
                          extend name (VThunk (ThunkRef cell)) env
                      else env
        match eval evalEnv body with
        | Ok v ->

            if isRec then
                match Map.tryFind name evalEnv with
                | Some (VThunk (ThunkRef cell)) -> cell := Evaluated v
                | _ -> ()
            Ok (extend name v env)
        | Error e -> Error e
    | DLet (_, _, _ :: _, _, _) ->
        Error (TypeError ("DLet with params should be desugared", None))
    | DType (name, _, variants, _) ->

        let env' =
            variants |> List.fold (fun e variant ->
                if List.isEmpty variant.Fields then
                    extend variant.Name (VCtor (variant.Name, [])) e
                else
                    let arity = List.length variant.Fields
                    let rec makeCtorFn (acc: Value list) remaining =
                        if remaining = 0 then
                            VCtor (variant.Name, List.rev acc)
                        else
                            VBuiltin (variant.Name, fun v ->
                                Ok (makeCtorFn (v :: acc) (remaining - 1)))
                    extend variant.Name (makeCtorFn [] arity) e
            ) env
        Ok env'
    | DImport _ -> Ok env

let runProgram (env: Env) (prog: Program) : Result<Env * Value option, GoatError> =
    let rec go env decls =
        match decls with
        | [] -> Ok (env, None)
        | d :: rest ->
            match evalDecl env d with
            | Error e -> Error e
            | Ok env' -> go env' rest
    match go env prog.Decls with
    | Error e -> Error e
    | Ok (env', _) ->
        match Map.tryFind "main" env' with
        | Some (VIO action) ->
            match action () with
            | IOOk v    -> Ok (env', Some v)
            | IOError m -> Error (IOErr m)
        | Some fn ->
            match applyValue fn VUnit noSpan with
            | Error e -> Error e
            | Ok (VIO action) ->
                match action () with
                | IOOk v    -> Ok (env', Some v)
                | IOError m -> Error (IOErr m)
            | Ok v -> Ok (env', Some v)
        | None   -> Ok (env', None)
