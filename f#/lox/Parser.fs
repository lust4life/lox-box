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

    let rec makeBinary left checkTypes rightMaker =
        match advanceIfMatch checkTypes with
        | Some operator ->
            let right = rightMaker ()
            let expr: Expr = Binary(left, operator, right)
            makeBinary expr checkTypes rightMaker
        | None -> left

    let rec makeLogical left checkTypes rightMaker =
        match advanceIfMatch checkTypes with
        | Some operator ->
            let right = rightMaker ()
            let expr: Expr = Logical(left, operator, right)
            makeLogical expr checkTypes rightMaker
        | None -> left

    let consume tokenType msg =
        advanceIfMatch [ tokenType ]
        |> Option.defaultWith (fun _ -> raiseParseError (currentToken ()) msg)


    let rec makeCallOrGet (callee: Expr) argMaker =
        advanceIfMatch [ LEFT_PAREN; DOT ]
        |> Option.map (function
            | tk when tk.tokenType = LEFT_PAREN ->
                let arguments = makeArguments argMaker [] |> List.rev
                let paren = consume RIGHT_PAREN "Expect ')' after arguments."
                let callee = Call(callee, arguments, paren)
                makeCallOrGet callee argMaker
            | _ ->
                let name = consume IDENTIFIER "Expect property name after '.'."
                let get = Get(callee, name)
                makeCallOrGet get argMaker)
        |> Option.defaultValue callee


    and makeArguments argMaker args =
        if isMatch RIGHT_PAREN then
            args
        else
            if args.Length >= 255 then
                lox.error (currentToken ()) "Can't have more than 255 arguments."

            let args = argMaker () :: args

            match advanceIfMatch [ COMMA ] with
            | Some _ -> makeArguments argMaker args
            | None -> args

    let rec makeParams (paramList: Token list) =
        if isMatch RIGHT_PAREN then
            paramList
        else
            if paramList.Length >= 255 then
                lox.error (currentToken ()) "Can't have more than 255 parameters."

            let param = consume IDENTIFIER "Expect parameter name."
            let paramList = param :: paramList

            match advanceIfMatch [ COMMA ] with
            | Some _ -> makeParams paramList
            | None -> paramList

    let synchronize () =
        while not (isAtEnd ()) && (currentToken().tokenType <> SEMICOLON) do
            advance ()

        advance ()


    let rec expression () = assignment ()

    and assignment () : Expr =
        // we need to take = as an binary operator, as we can't forseen the later assign operator
        let lv = logicOr ()

        match advanceIfMatch [ EQUAL ] with
        | Some equalTk ->
            match lv with
            | :? Variable as lv ->
                let rv = assignment ()
                Assign(lv.name, rv)
            | :? Get as lv ->
                let rv = assignment ()
                Set(lv.callee, lv.name, rv)
            | _ -> raiseParseError equalTk "Invalid assignment target."
        | None -> lv

    and logicOr () : Expr =
        let left = logicAnd ()
        makeLogical left [ OR ] logicAnd

    and logicAnd () =
        let left = equality ()
        makeLogical left [ AND ] equality

    and equality () =
        let left = comparison ()
        makeBinary left [ BANG_EQUAL; EQUAL_EQUAL ] comparison

    and comparison () =
        let left = term ()
        makeBinary left [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ] term

    and term () =
        let left = factor ()
        makeBinary left [ MINUS; PLUS ] factor

    and factor () =
        let left = unary ()
        makeBinary left [ SLASH; STAR ] unary

    and unary () : Expr =
        match advanceIfMatch [ BANG; MINUS ] with
        | Some operator ->
            let right = unary ()
            Unary(operator, right)
        | None -> call ()

    and call () : Expr =
        let callee = primary ()
        makeCallOrGet callee expression

    and primary () : Expr =
        let tk = currentToken ()
        let tokenType = tk.tokenType

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
            Literal(tk.literal)
        | LEFT_PAREN ->
            advance ()
            let expr = expression ()
            consume RIGHT_PAREN "Expect ')' after expression." |> ignore
            Grouping(expr)
        | THIS ->
            advance ()
            This(tk)
        | SUPER ->
            advance ()
            consume DOT "Expect '.' after 'super'." |> ignore
            let method = consume IDENTIFIER "Expect superclass method name."
            Super(tk, method)
        | IDENTIFIER ->
            advance ()
            Variable(tk)
        | _ -> raiseParseError tk "Expect expression."

    and statement () : Stmt =
        printStmt ()
        |> Option.orElseWith ifStmt
        |> Option.orElseWith whileStmt
        |> Option.orElseWith forStmt
        |> Option.orElseWith returnStmt
        |> Option.orElseWith block
        |> Option.defaultWith exprStmt

    and returnStmt () =
        advanceIfMatch [ RETURN ]
        |> Option.map (fun tk ->
            let expr = if isMatch SEMICOLON then None else Some(expression ())
            consume SEMICOLON "Expect ';' after return." |> ignore
            Return(tk, expr))

    and block () =
        advanceIfMatch [ LEFT_BRACE ]
        |> Option.map (fun _ ->
            let stmts =
                [ while not (isAtEnd ()) && not (isMatch RIGHT_BRACE) do
                      match declaration () with
                      | Some stmt -> yield stmt
                      | None -> () ]

            consume RIGHT_BRACE "Expect '}' after block." |> ignore
            Block(stmts))

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

    and ifStmt () =
        advanceIfMatch [ IF ]
        |> Option.map (fun _ ->
            consume LEFT_PAREN "Expect '(' after 'if'." |> ignore
            let condition = expression ()
            consume RIGHT_PAREN "Expect ')' after if condition." |> ignore
            let thenPart = statement ()
            let elsePart = advanceIfMatch [ ELSE ] |> Option.map (fun _ -> statement ())
            If(condition, thenPart, elsePart))

    and whileStmt () =
        advanceIfMatch [ WHILE ]
        |> Option.map (fun _ ->
            consume LEFT_PAREN "Expect '(' after 'while'." |> ignore
            let condition = expression ()
            consume RIGHT_PAREN "Expect ')' after condition." |> ignore
            let body = statement ()
            While(condition, body))

    and forStmt () =
        advanceIfMatch [ FOR ]
        |> Option.map (fun _ ->
            consume LEFT_PAREN "Expect '(' after 'for'." |> ignore

            let initializer =
                match advanceIfMatch [ SEMICOLON ] with
                | Some _ -> None
                | None -> varDecl () |> Option.defaultWith exprStmt |> Some

            let condition: Expr =
                match advanceIfMatch [ SEMICOLON ] with
                | Some _ -> Literal(true)
                | None ->
                    let expr = expression ()
                    consume SEMICOLON "Expect ';' after loop condition.." |> ignore
                    expr

            let increment =
                match advanceIfMatch [ RIGHT_PAREN ] with
                | Some _ -> None
                | None ->
                    let expr = expression ()
                    consume RIGHT_PAREN "Expect ')' after for clauses." |> ignore
                    Some expr

            let body = statement ()

            let desugared =
                [ match initializer with
                  | Some initializer -> yield initializer
                  | None -> ()

                  let whileBody: Stmt =
                      match increment with
                      | Some increment -> Block([ body; Expression(increment) ])
                      | None -> body

                  yield While(condition, whileBody) ]

            Block(desugared))

    and declaration () =
        try
            varDecl ()
            |> Option.orElseWith funDeclar
            |> Option.orElseWith classDeclar
            |> Option.defaultWith statement
            |> Some
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

    and funDeclar () =
        advanceIfMatch [ FUN ] |> Option.map (fun _ -> FunDeclar(func "function"))

    and classDeclar () =
        advanceIfMatch [ CLASS ]
        |> Option.map (fun _ ->
            let name = consume IDENTIFIER "Expect class name."

            let superclass =
                advanceIfMatch [ LESS ]
                |> Option.map (fun _ -> consume IDENTIFIER "Expect superclass name.")

            consume LEFT_BRACE "Expect '{' before class body." |> ignore

            let methods =
                [ while not (isMatch RIGHT_BRACE) && not (isAtEnd ()) do
                      yield func "method" ]

            consume RIGHT_BRACE "Expect '}' after class body." |> ignore

            Class(name, methods, superclass))

    and func (kind: string) =
        let name = consume IDENTIFIER $"Expect {kind} name."
        consume LEFT_PAREN $"Expect '(' after {kind}" |> ignore
        let paramList = makeParams [] |> List.rev
        consume RIGHT_PAREN "Expect ')' after parameters." |> ignore

        let body =
            block ()
            |> Option.map (function
                | :? Block as block -> block.stmts
                | _ -> failwith "should not happen, must be a block here")
            |> Option.defaultWith (fun _ -> raiseParseError (currentToken ()) $"Expect '{{' before {kind} body.")

        { name = name
          paramList = paramList
          body = body }

    member x.parse() =
        seq {
            while not (isAtEnd ()) do
                match declaration () with
                | Some stmt -> yield stmt
                | None -> ()
        }

    member x.parseExpr() = expression ()
