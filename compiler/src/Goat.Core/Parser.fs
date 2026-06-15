module Goat.Core.Parser

open FParsec
open Goat.Core.Ast
open Goat.Core.Lexer

let getSpan (startPos: Position) (endPos: FParsec.Position) : Span =
    let toPos (p: FParsec.Position) =
        { Line = int p.Line; Column = int p.Column; File = p.StreamName }
    { Start = startPos; End = toPos endPos }

let withSpan (p: Parser<Ast.Position -> 'a, unit>) : Parser<'a, unit> =
    pipe2
        (getPosition |>> fun fp -> { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName })
        p
        (fun startPos mkNode -> mkNode startPos)

let spanOf (p: Parser<'a, unit>) (f: 'a -> Ast.Position -> 'b) : Parser<'b, unit> =
    pipe3
        (getPosition |>> fun fp -> { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName })
        p
        (getPosition |>> fun fp -> { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName })
        (fun sp x ep -> f x sp)

let expr, exprRef     = createParserForwardedToRef<Expr, unit>()
let pattern, patRef   = createParserForwardedToRef<Pattern, unit>()

let litParser : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        let mkSpan () = { Start = sp; End = sp }
        choice [
            attempt (intLit    .>> ws |>> fun i -> Lit (LInt i,    mkSpan ()))
            attempt (floatLit  .>> ws |>> fun f -> Lit (LFloat f,  mkSpan ()))
            attempt (boolLit   .>> ws |>> fun b -> Lit (LBool b,   mkSpan ()))
            attempt (stringLit .>> ws |>> fun s -> Lit (LString s, mkSpan ()))
            attempt (unitLit   .>> ws >>. preturn (Lit (LUnit,     mkSpan ())))
        ]

let patAtom : Parser<Pattern, unit> =
    choice [
        attempt (pchar '_' >>. ws >>. preturn PWild)
        attempt (unitLit >>. ws >>. preturn (PLit LUnit))
        attempt (intLit   .>> ws |>> fun i -> PLit (LInt i))
        attempt (floatLit .>> ws |>> fun f -> PLit (LFloat f))
        attempt (boolLit  .>> ws |>> fun b -> PLit (LBool b))
        attempt (stringLit .>> ws |>> fun s -> PLit (LString s))

        attempt (upperIdent .>> ws >>= fun name ->
            choice [
                attempt (between lparen rparen (sepBy pattern comma) |>> fun ps ->
                    PCtor (name, ps))

                attempt (many1 (choice [
                    attempt (pchar '_' >>. ws >>. preturn PWild)
                    attempt (intLit   .>> ws |>> fun i -> PLit (LInt i))
                    attempt (floatLit .>> ws |>> fun f -> PLit (LFloat f))
                    attempt (boolLit  .>> ws |>> fun b -> PLit (LBool b))
                    attempt (stringLit .>> ws |>> fun s -> PLit (LString s))
                    attempt (unitLit >>. ws >>. preturn (PLit LUnit))
                    attempt (between lparen rparen (sepBy1 pattern comma) |>> fun ps ->
                        match ps with [p] -> p | _ -> PTuple ps)
                    attempt (ident .>> ws |>> PVar)
                ]) |>> fun ps -> PCtor (name, ps))
                preturn (PCtor (name, []))
            ])

        attempt (between lparen rparen (sepBy1 pattern comma) |>> fun ps ->
            match ps with
            | [p] -> p
            | _   -> PTuple ps)

        attempt (between lbrack rbrack (sepBy pattern comma) |>> PList)

        attempt (ident .>> ws |>> PVar)
    ]

let patCons : Parser<Pattern, unit> =
    sepBy1 patAtom colon2 |>> fun ps ->
        List.foldBack (fun p acc -> PCons (p, acc)) (List.take (List.length ps - 1) ps) (List.last ps)

do patRef :=
    patCons >>= fun p ->
        opt (kw "as" >>. ident .>> ws) |>> fun alias ->
            match alias with
            | Some name -> PAs (p, name)
            | None      -> p

let param : Parser<Param, unit> =
    choice [
        attempt (pchar '_' >>. ws >>. preturn PWild_)
        attempt (between lparen rparen
                    (pipe2 (ident .>> ws .>> sym ":") (ws >>. ident .>> ws)
                        (fun name t -> PAnn (name, TName t))))
        attempt (ident .>> ws |>> PSimple)
    ]

let ioStmt : Parser<IOStmt, unit> =
    choice [
        attempt (kw "let" >>. pchar '!' >>. ws >>. ident .>> ws .>> eq >>= fun name ->
            expr |>> fun e -> IOLet (name, e))
        attempt (kw "let" >>. ident .>> ws .>> eq >>= fun name ->
            expr |>> fun e -> IOLetPure (name, e))
        attempt (kw "do" >>. pchar '!' >>. ws >>. expr |>> IODo)
        attempt (kw "return" >>. expr |>> IOReturn)
    ]

let ioBlock : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        kw "io" >>. between lbrace rbrace (many1 (ioStmt .>> ws .>> opt (pchar ';' >>. ws))) |>> fun stmts ->
            IOBlock (stmts, { Start = sp; End = sp })

let matchArm : Parser<MatchArm, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        pipe2
            (pattern .>> ws >>= fun p ->
                opt (kw "when" >>. expr) |>> fun g -> (p, g))
            (fatArrow >>. expr)
            (fun (p, guard) body ->
                { Pattern = p; Guard = guard; Body = body; Span = { Start = sp; End = sp } })

let listExpr : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        between lbrack rbrack (
            attempt (pipe2 expr (dotdot >>. expr) (fun lo hi ->
                Range (lo, hi, { Start = sp; End = sp })))
            <|>
            (sepBy expr comma |>> fun es -> ListLit (es, { Start = sp; End = sp }))
        )

let atomExpr : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        choice [
            attempt ioBlock
            attempt litParser
            attempt (kw "lazy"  >>. expr |>> fun e -> Lazy  (e, { Start = sp; End = sp }))
            attempt (kw "force" >>. expr |>> fun e -> Force (e, { Start = sp; End = sp }))
            attempt (upperIdent .>> ws |>> fun name -> Ctor (name, [], { Start = sp; End = sp }))
            attempt listExpr
            attempt (between lparen rparen (sepBy1 expr comma) |>> fun es ->
                match es with
                | [e] -> e
                | _   -> Tuple (es, { Start = sp; End = sp }))
            attempt (ident .>> ws |>> fun name -> Var (name, { Start = sp; End = sp }))
        ]

let appExpr : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        many1 atomExpr |>> fun exprs ->
            List.reduce (fun f a -> App (f, a, { Start = sp; End = sp })) exprs

let opp = OperatorPrecedenceParser<Expr, unit, unit>()

let addInfix sym prec assoc op =
    opp.AddOperator(InfixOperator(sym, ws, prec, assoc, fun l r ->
        BinOp (op, l, r, noSpan)))

let addPrefix sym prec op =
    opp.AddOperator(PrefixOperator(sym, ws, prec, true, fun e ->
        UnOp (op, e, noSpan)))

do
    addInfix "|>"  1 Associativity.Left  Pipe
    addInfix "~>"  1 Associativity.Left  LazyPipe
    addInfix "||"  2 Associativity.Left  Or
    addInfix "&&"  3 Associativity.Left  And
    addPrefix "not" 4 Not
    addInfix "==" 5 Associativity.Left  Eq
    addInfix "!=" 5 Associativity.Left  NEq
    addInfix "/=" 5 Associativity.Left  NEq
    addInfix "<=" 5 Associativity.Left  LEq
    addInfix ">=" 5 Associativity.Left  GEq
    addInfix "<"  5 Associativity.Left  Lt
    addInfix ">"  5 Associativity.Left  Gt
    addInfix "++" 6 Associativity.Right Append
    opp.AddOperator(InfixOperator("::", ws, 7, Associativity.Right, fun h t ->
        Cons (h, t, noSpan)))
    addInfix "+" 8 Associativity.Left  Add
    addInfix "-" 8 Associativity.Left  Sub
    addInfix "*"   9 Associativity.Left  Mul
    addInfix "/"   9 Associativity.Left  Div
    addInfix "%"   9 Associativity.Left  Mod
    opp.AddOperator(InfixOperator("mod", ws, 9, Associativity.Left, fun l r ->
        BinOp (Mod, l, r, noSpan)))
    addInfix "**" 10 Associativity.Right Pow
    addPrefix "-" 11 Neg

opp.TermParser <- appExpr .>> ws

let letExpr : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        pipe3
            (kw "let" >>. opt (kw "rec") .>>. ident .>> ws .>>. many param .>> eq)
            expr
            (kw "in" >>. expr)
            (fun ((isRec, name), parms) body cont ->
                Let (Option.isSome isRec, name, parms, body, cont, { Start = sp; End = sp }))

let funExpr : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        sym "\\" >>. many1 param .>> arrow >>= fun parms ->
            expr |>> fun body -> Lam (parms, body, { Start = sp; End = sp })

let ifExpr : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        pipe3
            (kw "if"   >>. expr)
            (kw "then" >>. expr)
            (kw "else" >>. expr)
            (fun cond t e -> If (cond, t, e, { Start = sp; End = sp }))

let matchExpr : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        pipe2
            (kw "match" >>. expr .>> kw "with")
            (opt pipe >>. pipe2 matchArm (many (attempt (pipe >>. matchArm))) (fun h t -> h :: t))
            (fun scrut arms -> Match (scrut, arms, { Start = sp; End = sp }))

let lazyExpr : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        kw "lazy" >>. expr |>> fun e -> Lazy (e, { Start = sp; End = sp })

let forceExpr : Parser<Expr, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        kw "force" >>. expr |>> fun e -> Force (e, { Start = sp; End = sp })

do exprRef :=
    ws >>. choice [
        attempt letExpr
        attempt funExpr
        attempt ifExpr
        attempt matchExpr
        attempt lazyExpr
        attempt forceExpr
        opp.ExpressionParser
    ]

let variantDef : Parser<VariantDef, unit> =
    pipe2
        (upperIdent .>> ws)
        (opt (kw "of" >>. many1 (attempt (ident .>> ws |>> TName))))
        (fun name fields -> { Name = name; Fields = Option.defaultValue [] fields })

let typeDecl : Parser<Decl, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        pipe2
            (kw "type" >>. ident .>> ws .>> eq .>> ws)
            (opt pipe >>. pipe2 variantDef (many (attempt (pipe >>. variantDef))) (fun h t -> h :: t))
            (fun name variants ->
                DType (name, [], variants, { Start = sp; End = sp }))

let topLet : Parser<Decl, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        pipe2
            (choice [
                attempt (kw "fun" >>. opt (kw "rec") .>>. ident .>> ws .>>. many1 param .>> eq
                         |>> fun ((isRec, name), parms) -> (Option.isSome isRec, name, parms))
                attempt (kw "let" >>. opt (kw "rec") .>>. ident .>> ws .>>. many param .>> eq
                         |>> fun ((isRec, name), parms) -> (Option.isSome isRec, name, parms))
            ])
            expr
            (fun (isRec, name, parms) body ->
                DLet (isRec, name, parms, body, { Start = sp; End = sp }))

let importDecl : Parser<Decl, unit> =
    getPosition >>= fun fp ->
        let sp = { Line = int fp.Line; Column = int fp.Column; File = fp.StreamName }
        kw "import" >>. stringLit .>> ws |>> fun path ->
            DImport (path, { Start = sp; End = sp })

let decl : Parser<Decl, unit> =
    ws >>. choice [
        attempt typeDecl
        attempt topLet
        attempt importDecl
    ]

let program : Parser<Program, unit> =
    ws >>. many decl .>> eof |>> fun decls -> { Decls = decls; Source = "" }

type ParseError = string

let private extractResult (r: FParsec.CharParsers.ParserResult<'a, unit>) : Result<'a, string> =
    match r with
    | Success (v, _, _)   -> Result.Ok v
    | Failure (msg, _, _) -> Result.Error msg

let parseProgram (source: string) (filename: string) : Result<Program, ParseError> =
    let r = runParserOnString (program .>> eof) () filename source
    match extractResult r with
    | Result.Ok prog -> Result.Ok { prog with Source = source }
    | Result.Error msg -> Result.Error msg

let parseExpr (source: string) : Result<Expr, ParseError> =
    let r = runParserOnString (ws >>. expr .>> eof) () "<expr>" source
    extractResult r
