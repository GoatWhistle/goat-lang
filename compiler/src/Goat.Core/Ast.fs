module Goat.Core.Ast

type Position = { Line: int; Column: int; File: string }
type Span     = { Start: Position; End: Position }

let noPos  = { Line = 0; Column = 0; File = "<unknown>" }
let noSpan = { Start = noPos; End = noPos }

type Ident = string

type Literal =
    | LInt    of int64
    | LFloat  of float
    | LBool   of bool
    | LString of string
    | LUnit

type TypeExpr =
    | TName   of Ident
    | TArrow  of TypeExpr * TypeExpr
    | TList   of TypeExpr
    | TTuple  of TypeExpr list
    | TApp    of TypeExpr * TypeExpr

type Pattern =
    | PWild
    | PLit   of Literal
    | PVar   of Ident
    | PCons  of Pattern * Pattern
    | PList  of Pattern list
    | PTuple of Pattern list
    | PCtor  of Ident * Pattern list
    | PAs    of Pattern * Ident

type BinOp =
    | Add | Sub | Mul | Div | Mod | Pow
    | Eq  | NEq | Lt  | Gt  | LEq | GEq
    | And | Or
    | Append
    | Pipe
    | LazyPipe

type UnOp =
    | Neg
    | Not

type Param =
    | PSimple of Ident
    | PAnn    of Ident * TypeExpr
    | PWild_

type IOStmt =
    | IODo      of Expr
    | IOLet     of Ident * Expr
    | IOLetPure of Ident * Expr
    | IOReturn  of Expr

and Expr =
    | Lit      of Literal * Span
    | Var      of Ident   * Span
    | Let      of isRec:bool * Ident * Param list * Expr * Expr * Span
    | Lam      of Param list * Expr * Span
    | App      of Expr * Expr * Span
    | BinOp    of BinOp * Expr * Expr * Span
    | UnOp     of UnOp  * Expr * Span
    | If       of Expr * Expr * Expr * Span
    | Match    of Expr * MatchArm list * Span
    | ListLit  of Expr list * Span
    | Cons     of Expr * Expr * Span
    | Tuple    of Expr list * Span
    | Lazy     of Expr * Span
    | Force    of Expr * Span
    | IOBlock  of IOStmt list * Span
    | Ctor     of Ident * Expr list * Span
    | Ann      of Expr * TypeExpr * Span
    | Range    of Expr * Expr * Span

and MatchArm =
    { Pattern : Pattern
      Guard   : Expr option
      Body    : Expr
      Span    : Span }

type TypeParam  = string
type VariantDef = { Name: Ident; Fields: TypeExpr list }

type Decl =
    | DLet    of isRec:bool * Ident * Param list * Expr * Span
    | DType   of Ident * TypeParam list * VariantDef list * Span
    | DImport of string * Span

type Program = { Decls: Decl list; Source: string }
