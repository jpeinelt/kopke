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