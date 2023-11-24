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

    let isMatch tokenType =
        let ct = currentToken ()
        ct.tokenType = tokenType

    let raiseParseError token msg =
        lox.error token msg
        raise (ParseError(token, msg))

    let advanceIfMatch (types: TokenType list) =
        types
        |> List.tryFind isMatch
        |> Option.map (fun _ ->
            let ct = currentToken ()
            advance ()
            ct)

    let rec tryMakeBinary left checkTypes rightMaker =
        match advanceIfMatch checkTypes with
        | Some operator ->
            let right = rightMaker ()
            let expr: Expr = Binary(left, operator, right)
            tryMakeBinary expr checkTypes rightMaker
        | None -> left

    let consume tokenType msg =
        advanceIfMatch [ tokenType ]
        |> Option.defaultWith (fun _ -> raiseParseError (currentToken ()) msg)

    let synchronize () =
        while not (isAtEnd ()) && (currentToken().tokenType <> SEMICOLON) do
            advance ()

        advance ()


    let rec expression () = assignment ()

    and assignment () =
        // we need to take = as an binary operator, as we can't forseen the later assign operator
        let lv = equality ()

        match advanceIfMatch [ EQUAL ] with
        | Some equalTk ->
            match lv with
            | Variable name ->
                let rv = assignment ()
                Assign(name, rv)
            | _ -> raiseParseError equalTk "Invalid assignment target."
        | None -> lv

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
            consume RIGHT_PAREN "Expect ')' after expression." |> ignore
            Grouping(expr)
        | IDENTIFIER ->
            advance ()
            Variable(ct)
        | _ -> raiseParseError ct "Expect expression."

    and statement () =
        printStmt () |> Option.defaultWith exprStmt

    and exprStmt () =
        let expr = expression ()
        consume SEMICOLON "Expect ';' after expression." |> ignore
        Expression(expr)

    and printStmt () =
        advanceIfMatch [ PRINT ]
        |> Option.map (fun _ ->
            let expr = expression ()
            consume SEMICOLON "Expect ';' after value." |> ignore
            Print(expr))

    and declaration () =
        try
            varDecl () |> Option.defaultWith statement |> Some
        with ParseError(_, _) ->
            synchronize ()
            None

    and varDecl () =
        advanceIfMatch [ VAR ]
        |> Option.map (fun _ ->
            let name = consume IDENTIFIER "Expect variable name."
            let expr = advanceIfMatch [ EQUAL ] |> Option.map (fun _ -> expression ())
            consume SEMICOLON "Expect ';' after variable declaration." |> ignore

            VarDeclar(name, expr))

    and identifier () = ()

    member x.parse() =
        seq {
            while not (isAtEnd ()) do
                match declaration () with
                | Some stmt -> yield stmt
                | None -> ()
        }

    member x.parseExpr() = expression ()
