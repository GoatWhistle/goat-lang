module Goat.Core.Lexer

open FParsec

let keywords =
    System.Collections.Generic.HashSet<string>(
        [| "let"; "rec"; "fun"; "in"; "if"; "then"; "else"
           "match"; "with"; "type"; "of"; "import"
           "lazy"; "force"; "true"; "false"
           "io"; "do"; "return"; "and"
           "not"; "mod" |])

let isKeyword s = keywords.Contains s

let lineComment : Parser<unit, unit> =
    skipString "--" >>. skipRestOfLine true

let blockComment : Parser<unit, unit> =
    let comment, commentRef = createParserForwardedToRef<unit, unit> ()
    let body =
        skipMany (
            comment
            <|> (notFollowedBy (skipString "-}") >>. notFollowedBy (skipString "{-") >>. skipAnyChar))
    commentRef.Value <- skipString "{-" >>. body >>. skipString "-}"
    comment

let ws : Parser<unit, unit> =
    skipMany (spaces1 <|> lineComment <|> blockComment)

let ws1 : Parser<unit, unit> =
    skipMany1 (spaces1 <|> lineComment <|> blockComment)

let isIdentStart c = System.Char.IsLetter c || c = '_'
let isIdentCont  c = System.Char.IsLetterOrDigit c || c = '_' || c = '\''

let rawIdent : Parser<string, unit> =
    many1Satisfy2L isIdentStart isIdentCont "identifier"

let ident : Parser<string, unit> =
    rawIdent >>= fun s ->
        if isKeyword s then
            fail $"'{s}' is a keyword"
        else
            preturn s

let upperIdent : Parser<string, unit> =
    many1Satisfy2L System.Char.IsUpper isIdentCont "constructor name"

let intLit : Parser<int64, unit> =
    opt (pchar '-') .>>. many1Satisfy System.Char.IsDigit
    .>> notFollowedBy (pchar '.' .>> notFollowedBy (pchar '.'))
    |>> fun (sign, digits) ->
        let s = (match sign with Some _ -> "-" | None -> "") + digits
        int64 s

let floatLit : Parser<float, unit> =
    pfloat

let boolLit : Parser<bool, unit> =
    (stringReturn "true" true) <|> (stringReturn "false" false)

let stringLit : Parser<string, unit> =
    let escape =
        pchar '\\' >>. choice [
            pchar 'n'  >>. preturn '\n'
            pchar 't'  >>. preturn '\t'
            pchar 'r'  >>. preturn '\r'
            pchar '\\' >>. preturn '\\'
            pchar '"'  >>. preturn '"'
            pchar '\'' >>. preturn '\''
        ]
    let strChar = escape <|> noneOf "\"\\"
    between (pchar '"') (pchar '"') (manyChars strChar)

let unitLit : Parser<unit, unit> =
    skipString "()" >>. preturn ()

let sym  s = skipString s >>. ws
let sym' s = skipString s

let kw   s = skipString s >>. (notFollowedBy (satisfy isIdentCont)) >>. ws

let lparen = sym "("
let rparen = sym ")"
let lbrack = sym "["
let rbrack = sym "]"
let lbrace = sym "{"
let rbrace = sym "}"
let comma  = sym ","
let pipe   = sym "|"
let arrow  = sym "->"
let fatArrow = sym "=>"
let eq     = sym "="
let bang   = sym "!"
let colon2 = sym "::"
let dotdot = sym ".."
