module Parser


open System
open System.Collections.Generic

open FParsec

open Ast


(* lexical definitions *)

let ws = spaces
let str s = pstring s >>. ws

let identifierString = many1Satisfy isLower .>> ws // [a-z]+
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

let numberLit : Parser<NumberLiteral, unit> = numberLiteral numberFormat "number" .>> ws

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

let declaration = str ":" >>. identifier |>> KDecl

// for recursive grammar productions we need a dummy
// expr is a parser that forward all calls to a reference
// call to break the cyclic dependency.
let expr, exprRef = createParserForwardedToRef()

let cond =
    // "Cond KExpr ?(alt1) ?(alt2) ?(alt3)...
    KAbort

let apply =
    KAbort

let prim =
    KAbort

let funcArgs = str ":" >>. identifier

let anonFunction =
    between (str "[") (str "]")
            (pipe2 (many funcArgs) (str "|" >>. expr)
                (fun args expr -> KAnonFun(args, expr)))

// replace dummy parser reference in exprRef
do exprRef:= choice [number; kbool; kstring; declaration; (*...*) anonFunction]