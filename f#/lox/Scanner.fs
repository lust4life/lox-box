namespace lox.scanner

open lox.token
open System
open lox

type Scanner(source: string) =
    let mutable eachStart = 0
    let mutable current = 0
    let mutable line = 1

    let keywords: Map<string, TokenType> =
        Map["and", AND
            "and", AND
            "class", CLASS
            "else", ELSE
            "false", FALSE
            "for", FOR
            "fun", FUN
            "if", IF
            "nil", NIL
            "or", OR
            "print", PRINT
            "return", RETURN
            "super", SUPER
            "this", THIS
            "true", TRUE
            "var", VAR
            "while", WHILE]

    let ended index = index >= source.Length

    let currentChar () = source[current]
    let currentLexeme () = source[eachStart..current]

    let advance () = current <- current + 1

    let rawToken tokenType =
        { tokenType = tokenType
          lexeme = currentLexeme ()
          literal = null
          line = line }

    let token tokenType literal =
        { tokenType = tokenType
          lexeme = currentLexeme ()
          literal = literal
          line = line }

    let checkIndex index checkFunc =
        not (ended index) && checkFunc (source[index])

    let matchNext one =
        checkIndex (current + 1) (fun next -> next = one)

    let advanceTill terminator =
        while not (matchNext terminator) && not (ended current) do
            if currentChar () = '\n' then
                line <- line + 1

            advance ()

    let handleOneMore next ifT notT =
        if matchNext next then
            advance ()
            rawToken ifT
        else
            rawToken notT

    let handleString () =
        seq {
            advanceTill '"'

            if matchNext '"' then
                advance ()
                let literalStr = source[eachStart + 1 .. current - 1]
                yield token STRING literalStr
            else
                lox.errorWithLine line "Unterminated string."
        }

    let isDigital char = char >= '0' && char <= '9'

    let handleNumber () =
        while checkIndex (current + 1) isDigital do
            advance ()

        if matchNext '.' && checkIndex (current + 2) isDigital then
            advance ()

            while checkIndex (current + 1) isDigital do
                advance ()

        let literalNumber = Double.Parse(currentLexeme ())
        token NUMBER literalNumber

    let isAlpha char =
        ('a' <= char && char <= 'z') || ('A' <= char && char <= 'Z') || char = '_'

    let isAlphaNumeric char = isAlpha char || isDigital char

    let handleKeywordsAndIdentifier () =
        while checkIndex (current + 1) isAlphaNumeric do
            advance ()

        let tokenType =
            keywords |> Map.tryFind (currentLexeme ()) |> Option.defaultValue IDENTIFIER

        rawToken tokenType

    member x.scanTokens() : Token seq =
        seq {
            while not (ended current) do
                eachStart <- current

                match currentChar () with
                | '(' -> yield rawToken LEFT_PAREN
                | ')' -> yield rawToken RIGHT_PAREN
                | '{' -> yield rawToken LEFT_BRACE
                | '}' -> yield rawToken RIGHT_BRACE
                | ',' -> yield rawToken COMMA
                | '.' -> yield rawToken DOT
                | '-' -> yield rawToken MINUS
                | '+' -> yield rawToken PLUS
                | ';' -> yield rawToken SEMICOLON
                | '*' -> yield rawToken STAR
                | '!' -> yield handleOneMore '=' BANG_EQUAL BANG
                | '=' -> yield handleOneMore '=' EQUAL_EQUAL EQUAL
                | '>' -> yield handleOneMore '=' GREATER_EQUAL GREATER
                | '<' -> yield handleOneMore '=' LESS_EQUAL LESS
                | '/' ->
                    if matchNext '/' then
                        advanceTill '\n'
                    else
                        yield rawToken SLASH
                | ' '
                | '\r'
                | '\t' -> ()
                | '\n' -> line <- line + 1
                | '"' -> yield! handleString ()
                | char when isDigital char -> yield handleNumber ()
                | char when isAlpha char -> yield handleKeywordsAndIdentifier ()
                | _ -> lox.errorWithLine line "Unexpected character."

                advance ()

            yield
                { tokenType = EOF
                  lexeme = ""
                  literal = null
                  line = line }
        }
