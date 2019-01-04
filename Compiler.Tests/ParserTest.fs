module Compiler.Tests

open System
open NUnit.Framework
open NUnit.Framework.Internal

open FParsec

open Parser
open Ast

type ParsingResult =
    | PSuccess of KExpr * bool
    | PFailure of string * bool

let parserTest p str =
    match run p str with
    | Success(v,_,_) -> v
    | Failure(_)     -> KAbort


[<TestFixture>]
type ParserTestClass () =

    (* numbers *)

    [<Test>]
    member public this.TestFloatNumber() =
        let expected = KFloat 3.1415
        let actual = parserTest number "3.1415"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestIntNumber() =
        let expected = KInt 3
        let actual = parserTest number "3"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestScientificdNumber() =
        let expected = KFloat 3e11
        let actual = parserTest number "3e11"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestNumberInvalid() =
        let expected = KAbort
        let actual = parserTest number "3ea"
        Assert.AreEqual(expected, actual)

    (* bool *)
    
    [<Test>]
    member public this.TestBool() =
        let expectedTrue = KBool true
        let actualTrue = parserTest kbool "True"
        Assert.AreEqual(expectedTrue, actualTrue)

        let expectedFalse = KBool (false)
        let actualFalse = parserTest kbool "False"
        Assert.AreEqual(expectedFalse, actualFalse)

    [<Test>]
    member public this.TestBoolInvalid() =
        let expectedTrue = KAbort
        let actualTrue = parserTest kbool "Treu"
        Assert.AreEqual(expectedTrue, actualTrue)
    
    (* string *)

    [<Test>]
    member public this.TestString() =
        let expected = KString "some \n 'fancy' str1ng"
        let actual = parserTest kstring "\"some \n 'fancy' str1ng\""
        Assert.AreEqual(expected, actual)
    
    
    (* declaration *)

    [<Test>]
    member public this.TestDeclaration() =
        let expected = KDecl "varname"
        let actual = parserTest declaration ":varname"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestDeclarationInvalid() =
        let expected = KAbort
        let actual = parserTest declaration "varname"
        Assert.AreEqual(expected, actual)


    (* identifier *)

    [<Test>]
    member public this.TestIdentifier() =
        let expected = KIdent "someidentifier"
        let actual = parserTest ident "someidentifier"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestIdentifierInvalid() =
        let expected = KAbort
        let actual = parserTest ident ":ident"
        Assert.AreEqual(expected, actual)
    
    [<Test>]
    member public this.TestIdentifierInvalidUpperCase() =
        let expected = KAbort
        let actual = parserTest ident "Identifier"
        Assert.AreEqual(expected, actual)

    (* conditions *)

    [<Test>]
    member public this.TestCondAlternativeWithBool() =
        let expected = KBool true
        let actual = parserTest condAlternative "?(True)"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestCondAlternativeWithIdentifier() =
        let expected = KIdent "identifier"
        let actual = parserTest condAlternative "?(identifier)"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestCondAlternativeNumber() =
        let expected = KInt 3
        let actual = parserTest condAlternative "?( 3 )"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestCondAlternativeWithString() =
        let expected = KString "yipeee"
        let actual = parserTest condAlternative "?(\"yipeee\")"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestCondOneAlternative() =
        let expected = KCond(KBool true, [KString "so true"])
        let actual = parserTest cond "Cond True ?(\"so true\")"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestCondTwoAlternatives() =
        let expected = KCond(KBool true, [KString "so true"; KBool false])
        let actual = parserTest cond "Cond True ?(\"so true\") ?( False )"
        Assert.AreEqual(expected, actual)


    (* apply *)

    [<Test>]
    member public this.TestApplyEmptyArgs() =
        let expected = KApply("somefunc", [])
        let actual = parserTest apply "somefunc()"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestApplyWithArgs() =
        let expected = KApply("somefunc", [KString "so true"; KBool false])
        let actual = parserTest apply "somefunc( \"so true\", False )"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestApplyFailing() =
        let expected = KAbort
        let actual = parserTest apply "somefunc( \"so true\",)" // trailing argument comma
        Assert.AreEqual(expected, actual)

    (* primitives *)

    [<Test>]
    member public this.TestPrimitiveWhitespace() =
        let expected = KPrim("+", KInt 4, KInt 5)
        let actual = parserTest prim "4 + 5"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestPrimitiveBoolOp() =
        let expected = KPrim("OR", KBool true, KBool false)
        let actual = parserTest prim "True OR False"
        Assert.AreEqual(expected, actual)

    (* functions *)

    [<Test>]
    member public this.TestFunctionEmpty() =
        let expected = KAnonFun([], [])
        let actual = parserTest anonFunction "[|]"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestFunctionNoArgs() =
        let expected = KAnonFun([], [KString "yeah"])
        let actual = parserTest anonFunction "[| \"yeah\"]"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestFunctionArgAndBody() =
        let expected = KAnonFun(["x"], [KString "yeah"])
        let actual = parserTest anonFunction "[:x| \"yeah\"]"
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.TestFunctionManyArgsAndBodyExprs() =
        let expected = KAnonFun(["x"; "y"], [KIdent "x"; KBool true; KIdent "y"])
        let actual = parserTest anonFunction "[:x :y | x True y ]"
        Assert.AreEqual(expected, actual)

    (* comments *)
    (* commands *)
    (* program *)