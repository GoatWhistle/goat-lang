module Goat.Core.Desugar

open Goat.Core.Ast

let rec private dsExpr (e: Expr) : Expr =
    match e with
    | BinOp (Pipe, l, r, sp) ->
        App (dsExpr r, dsExpr l, sp)

    | BinOp (LazyPipe, l, r, sp) ->
        App (dsExpr r, Lazy (dsExpr l, sp), sp)

    | Lam (parms, body, sp) ->
        Lam (parms, dsExpr body, sp)

    | Let (isRec, name, parms, body, cont, sp) ->
        let body' =
            if List.isEmpty parms then dsExpr body
            else Lam (parms, dsExpr body, sp)
        Let (isRec, name, [], body', dsExpr cont, sp)

    | App (f, a, sp)         -> App (dsExpr f, dsExpr a, sp)
    | BinOp (op, l, r, sp)   -> BinOp (op, dsExpr l, dsExpr r, sp)
    | UnOp  (op, e, sp)      -> UnOp  (op, dsExpr e, sp)
    | If (c, t, e, sp)       -> If (dsExpr c, dsExpr t, dsExpr e, sp)
    | Cons (h, t, sp)        -> Cons (dsExpr h, dsExpr t, sp)
    | ListLit (es, sp)       -> ListLit (List.map dsExpr es, sp)
    | Tuple (es, sp)         -> Tuple (List.map dsExpr es, sp)
    | Lazy (inner, sp)       -> Lazy (dsExpr inner, sp)
    | Force (inner, sp)      -> Force (dsExpr inner, sp)
    | Ctor (name, args, sp)  -> Ctor (name, List.map dsExpr args, sp)
    | Ann (e, t, sp)         -> Ann (dsExpr e, t, sp)
    | Range (lo, hi, sp)     -> Range (dsExpr lo, dsExpr hi, sp)
    | IOBlock (stmts, sp)    -> IOBlock (List.map dsStmt stmts, sp)
    | Match (scrut, arms, sp)->
        Match (dsExpr scrut, List.map dsArm arms, sp)
    | Lit _ | Var _ -> e

and private dsStmt (s: IOStmt) : IOStmt =
    match s with
    | IODo e          -> IODo (dsExpr e)
    | IOLet (n, e)    -> IOLet (n, dsExpr e)
    | IOLetPure (n,e) -> IOLetPure (n, dsExpr e)
    | IOReturn e      -> IOReturn (dsExpr e)

and private dsArm (arm: MatchArm) : MatchArm =
    { arm with
        Guard = Option.map dsExpr arm.Guard
        Body  = dsExpr arm.Body }

let private dsDecl (d: Decl) : Decl =
    match d with
    | DLet (isRec, name, parms, body, sp) ->
        let body' =
            if List.isEmpty parms then dsExpr body
            else Lam (parms, dsExpr body, sp)
        DLet (isRec, name, [], body', sp)
    | DType _ | DImport _ -> d

let desugar (prog: Program) : Program =
    { prog with Decls = List.map dsDecl prog.Decls }

let dsExpr' = dsExpr
