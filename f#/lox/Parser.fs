namespace lox.parser

open lox.token
open lox.expr

exception ParseError of Token * string

type Parser(tokens: Token list) =
    let mutable current = 0

    let currentToken () = tokens[current]

    let isAtEnd () = current = tokens.Length - 1

    let advance () =
        if (not (isAtEnd ())) then
            current <- current + 1

    let check tokenType =
        let ct = currentToken ()
        ct.tokenType = tokenType

    let consume tokenType msg =
        if check tokenType then
            advance ()
        else
            raise (ParseError(currentToken (), msg))


module parser =
    type ParserState = { tokens: Token list; current: int }


    let currentToken (state: ParserState) =
        let tk = state.tokens[state.current]
        tk

    let advance (state: ParserState) =
        { state with
            current = state.current + 1 }

    let check (state: ParserState) tokenType =
        let ct = currentToken state
        ct.tokenType = tokenType

    let consume (state: ParserState) tokenType msg =
        if check state tokenType then
            advance state
        else
            raise (ParseError(currentToken state, msg))

    let advanceIfMatch (state: ParserState) (types: TokenType list) =
        let ct = currentToken state

        types
        |> List.tryFind (fun x -> x = ct.tokenType)
        |> Option.map (fun _ -> ct, advance state)


    let rec expression (state: ParserState) = equality state

    and tryMakeBinary left checkTypes rightMaker state =
        match advanceIfMatch state checkTypes with
        | Some(operator, state) ->
            let right = rightMaker state
            let expr = Binary(left, operator, right)
            tryMakeBinary expr checkTypes rightMaker state
        | None -> left

    and equality (state: ParserState) : Expr =
        let left = comparison state
        tryMakeBinary left [ BANG_EQUAL; EQUAL_EQUAL ] comparison state

    and comparison (state: ParserState) =
        let left = term state
        tryMakeBinary left [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ] term state

    and term (state: ParserState) =
        let left = factor state
        tryMakeBinary left [ MINUS; PLUS ] factor state

    and factor (state: ParserState) =
        let left = unary state
        tryMakeBinary left [ SLASH; STAR ] unary state

    and unary (state: ParserState) =
        match advanceIfMatch state [ BANG; MINUS ] with
        | Some(tk, state) ->
            let right = unary state
            Unary(tk, right)
        | None -> primary state

    and primary (state: ParserState) =
        let token = currentToken state
        let tokenType = token.tokenType

        match tokenType with
        | TRUE -> Literal(true)
        | FALSE -> Literal(false)
        | NIL -> Literal(null)
        | NUMBER
        | STRING -> Literal(token.literal)
        | LEFT_PAREN ->
            let expr = expression state
            consume state RIGHT_BRACE "Expect ')' after expression." |> ignore
            Grouping(expr)
        | _ -> raise (ParseError(token, "Expect expression."))
