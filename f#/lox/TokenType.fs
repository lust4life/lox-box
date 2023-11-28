namespace lox.token

open System

type TokenType =

    // Single-character tokens.
    | LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR

    // One or two character tokens.
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL

    // Literals.
    | IDENTIFIER
    | STRING
    | NUMBER

    // Keywords.
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE

    | EOF

[<ReferenceEquality>]
type Token =
    { tokenType: TokenType
      lexeme: string
      literal: obj
      line: int }

    override x.ToString() =
        let literalStr =
            match x.literal with
            | null -> "null"
            | :? double as number when Double.IsInteger(number) -> sprintf "%.1f" number
            | other -> other.ToString()

        $"{x.tokenType} {x.lexeme} {literalStr}"
