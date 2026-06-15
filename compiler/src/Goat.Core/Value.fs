module Goat.Core.Value

open Goat.Core.Ast

type Env = Map<Ident, Value>

and Thunk =
    | Unevaluated of Env * Expr
    | Evaluated   of Value
    | BlackHole

and ThunkRef = ThunkRef of Thunk ref

and Value =
    | VInt     of int64
    | VFloat   of float
    | VBool    of bool
    | VString  of string
    | VUnit

    | VNil
    | VCons    of Value * ThunkRef

    | VTuple   of Value list

    | VClosure of Env * Param list * Expr

    | VBuiltin of string * (Value -> EvalResult)

    | VCtor    of string * Value list

    | VThunk   of ThunkRef

    | VIO      of (unit -> IOResult)

and EvalResult = Result<Value, GoatError>

and IOResult =
    | IOOk    of Value
    | IOError of string

and GoatError =
    | TypeError      of string * Span option
    | NameError      of string * Span option
    | MatchError     of string * Span option
    | DivisionByZero of Span option
    | IOErr          of string
    | StackOverflow_
    | UserError      of string
    | ArityError     of string * Span option

let makeThunk (env: Env) (expr: Expr) : Value =
    VThunk (ThunkRef (ref (Unevaluated (env, expr))))

let makeCons (head: Value) (env: Env) (tailExpr: Expr) : Value =
    VCons (head, ThunkRef (ref (Unevaluated (env, tailExpr))))

let typeNameOf (v: Value) =
    match v with
    | VInt _     -> "Int"
    | VFloat _   -> "Float"
    | VBool _    -> "Bool"
    | VString _  -> "String"
    | VUnit      -> "Unit"
    | VNil       -> "List"
    | VCons _    -> "List"
    | VTuple _   -> "Tuple"
    | VClosure _ -> "Function"
    | VBuiltin _ -> "Function"
    | VCtor(t,_) -> t
    | VThunk _   -> "Thunk"
    | VIO _      -> "IO"
