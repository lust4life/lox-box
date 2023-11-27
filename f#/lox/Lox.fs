namespace lox

open lox.token

exception RuntimeError of Token * string


module lox =
    let mutable hadError = false
    let mutable hadRuntimeError = false

    let runtimeError (error: RuntimeError) =
        eprintfn "%s\n[line %i]" error.Data1 error.Data0.line
        hadRuntimeError <- true

    let private report line where msg =
        eprintfn "[line %i] Error%s: %s" line where msg
        hadError <- true

    let error (token: Token) msg =
        if token.tokenType = EOF then
            report token.line " at end" msg
        else
            report token.line $" at '{token.lexeme}'" msg


    let errorWithLine (line: int) msg = report line "" msg
