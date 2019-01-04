module Parser


open System
open System.Collections.Generic

open FParsec

open Ast


(* lexical definitions *)

let ws = spaces
let skip s = pstring s >>. ws

let identifierString = many1Satisfy isAsciiLower // [a-z]+
let keywords = [ "Cond"; "True"; "False" ]
let keywordSet = new HashSet<string>(keywords)
let isKeyword str = keywordSet.Contains(str)

let identifier : Parser<string, unit> =
    let expectedIdentifier = expected "identifier"
    fun stream ->
        let state = stream.State
        let reply = identifierString stream
        if reply.Status <> Ok || not (isKeyword reply.Result) then reply
        else // Result is keyword, so backtrack to before the string
            stream.BacktrackTo(state)
            Reply(Error, expectedIdentifier)

let numberFormat =     NumberLiteralOptions.AllowMinusSign
                   ||| NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowExponent

let numberLit : Parser<NumberLiteral, unit> = numberLiteral numberFormat "number"

(* Parsers for Grammar productions *)

let number =
    numberLit
    |>> fun nl -> // An overflow will throw an exception
            if nl.IsInteger then KInt (int32 nl.String)
            else KFloat (float nl.String)

let kbool : Parser<KExpr, unit> =     (stringReturn "True"  (KBool true))
                                  <|> (stringReturn "False" (KBool false))


let kstring : Parser<KExpr, unit> =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))
    |>> KString

let declaration = pstring ":" >>. identifier |>> KDecl

let ident = identifier |>> KIdent

// for recursive grammar productions we need a dummy
// expr is a parser that forward all calls to a reference
// call to break the cyclic dependency.
let expr, exprRef = createParserForwardedToRef()
let condExpr, condExprRef = createParserForwardedToRef()
let applyExpr, applyRef = createParserForwardedToRef()

let condAlternative =
    between (skip "?(") (skip ")")
            (expr .>> ws)

let cond =
    pipe2 (skip "Cond" >>. condExpr .>> ws)
            (many1 (condAlternative <?> "?( expression )" .>> ws))
        (fun con alt -> KCond(con, alt))
do condExprRef:= choice [kbool]

let applyParameters =
    between (skip "(") (ws >>. skip ")")
            (sepBy applyExpr (pstring "," .>> ws))

let apply =
    pipe2 identifier (applyParameters <?> "comma-separated parameters")
        (fun funcIdent args -> KApply(funcIdent, args))

let prim =
    pipe3 (expr .>> ws)
        (choice [pstring "+"; pstring "-";
                  pstring "*"; pstring "/";
                  pstring "<"; pstring ">";
                  pstring "=="; pstring "!=";
                  pstring "OR"; pstring "AND";
                  pstring "NOT"] .>> ws)
            (expr .>> ws)
        (fun lexpr op rexpr -> KPrim(op, lexpr, rexpr))

let funcArgs = (many ((pstring ":" >>. identifier) .>> ws)) .>> (skip "|")

let funcBody = many (expr .>> ws)

let anonFunction =
    between (skip "[") (skip "]")
            (pipe2 (funcArgs <?> ":declarations")
                   (funcBody <?> "function body")
                (fun args body -> KAnonFun(args, body)))

// replace dummy parser references
do condExprRef:= choice [kbool; ident; apply(*; prim; anonFunction*)]
do exprRef:= choice [number; kbool; kstring; declaration; 
                    ident; cond; apply(*; prim; anonFunction*)]
do applyRef:= choice [number; kbool; kstring;
                    ident; apply(*; prim; anonFunction*)]
