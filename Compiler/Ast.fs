module Ast

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type KExpr =
    | KInt of int
    | KFloat of float
    | KBool of bool
    | KString of string
    | KDecl of string
    | KIdent of string
    | KPrim of string * KExpr * KExpr     // Primitive like + - * /
    | KAnonFun of string list * KExpr list
    | KCond of KExpr * KExpr list
    | KApply of string * KExpr list
    | KAbort // should be only used in tests
    with
        member private t.StructuredFormatDisplay =
            match t with
            | KInt    i -> box i
            | KFloat  f -> box f
            | KBool   b -> box b
            | KString s -> box ("\"" + s + "\"")
            | KDecl   s -> box (":" + s)
            | KIdent  s -> box ("@" + s)
            | KPrim (s, e1, e2) -> box s
            | KAnonFun (sl, el) -> box "function"
            | KCond (c, a) -> box "cond"
            | KApply (s, e) -> box ("function:" + s)
            | KAbort    -> box "abort"

type Command =
    | Expr of KExpr
    | Assign of string * KExpr

type Prog =
    | Cmmds of Command list