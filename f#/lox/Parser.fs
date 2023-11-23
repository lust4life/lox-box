namespace lox.parser

open lox
open lox.token
open lox.expr
open lox.stmt

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

    let parseError token msg =
        lox.error token msg
        ParseError(token, msg)

    let consume tokenType msg =
        if check tokenType then
            advance ()
        else
            raise (parseError (currentToken ()) msg)

    let advanceIfMatch (types: TokenType list) =
        let ct = currentToken ()

        types
        |> List.tryFind (fun x -> x = ct.tokenType)
        |> Option.map (fun _ ->
            advance ()
            ct)

    let rec statement () =
        match advanceIfMatch [ PRINT ] with
        | Some _ ->
            let expr = expression ()
            consume SEMICOLON "Expect ';' after value."
            Print(expr)
        | None ->
            let expr = expression ()
            consume SEMICOLON "Expect ';' after expression."
            Expression(expr)

    and expression () = equality ()

    and tryMakeBinary left checkTypes rightMaker =
        match advanceIfMatch checkTypes with
        | Some operator ->
            let right = rightMaker ()
            let expr = Binary(left, operator, right)
            tryMakeBinary expr checkTypes rightMaker
        | None -> left

    and equality () =
        let left = comparison ()
        tryMakeBinary left [ BANG_EQUAL; EQUAL_EQUAL ] comparison

    and comparison () =
        let left = term ()
        tryMakeBinary left [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ] term

    and term () =
        let left = factor ()
        tryMakeBinary left [ MINUS; PLUS ] factor

    and factor () =
        let left = unary ()
        tryMakeBinary left [ SLASH; STAR ] unary

    and unary () =
        match advanceIfMatch [ BANG; MINUS ] with
        | Some operator ->
            let right = unary ()
            Unary(operator, right)
        | None -> primary ()

    and primary () =
        let ct = currentToken ()
        let tokenType = ct.tokenType

        match tokenType with
        | TRUE ->
            advance ()
            Literal(true)
        | FALSE ->
            advance ()
            Literal(false)
        | NIL ->
            advance ()
            Literal(null)
        | NUMBER
        | STRING ->
            advance ()
            Literal(ct.literal)
        | LEFT_PAREN ->
            advance ()
            let expr = expression ()
            consume RIGHT_BRACE "Expect ')' after expression."
            Grouping(expr)
        | _ -> raise (parseError ct "Expect expression.")

    member x.parse() =
        seq {
            while not (isAtEnd ()) do
                yield (statement ())
        }

// try
//     Some(expression ())
// with ParseError(tk, msg) ->
//     None
