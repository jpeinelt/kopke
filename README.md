# Kopke Programming Language

Work in progress for a functional programming language with a strong smalltalk influence.

The plan so far is to first have it as an AST interpreter written in F#...


## Syntax

Int:

`1, 2, 100, 24`

Float:

`3.1415`

String:

`"This is a string"`

Function Definition:

`addition := [:x :y | x + y]`

Function Call:

`nextmove(currentplayer, board)`

if:

`Cond True ?("soooo true") ?("how could that be?")`


## Future

If I ever find the time, I want to implement maybe a simple type system and a CSP inspired concurrency model. A proper compiler for the .NET CLR/DLR would be nice too.
