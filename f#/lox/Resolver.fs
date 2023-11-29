namespace lox.resolver

open lox.interpreter
open lox.stmt
open lox.expr
open System.Collections.Generic
open lox.token
open lox

type FunctionType =
    | NONE
    | FUNCTION
    | INITIALIZER
    | METHOD

type ClassType =
    | NONE
    | CLASS

type Resolver(interpreter: Interpreter) =

    let scopes = Stack<Dictionary<string, bool>>()
    let mutable currentFunctionType = FunctionType.NONE
    let mutable currentClassType = ClassType.NONE

    let declare (name: Token) =
        match scopes.TryPeek() with
        | true, scope ->
            if scope.ContainsKey name.lexeme then
                lox.error name "Already a variable with this name in this scope."

            scope[name.lexeme] <- false
        | _ -> ()

    let defineByName name =
        match scopes.TryPeek() with
        | true, scope -> scope[name] <- true
        | _ -> ()

    let define (tk: Token) = defineByName tk.lexeme

    let createScope () =
        let scope = Dictionary<_, _>()
        scopes.Push scope

        { new System.IDisposable with
            member x.Dispose() = scopes.Pop() |> ignore }

    let withinFunction funcType =
        let previous = currentFunctionType
        currentFunctionType <- funcType

        { new System.IDisposable with
            member x.Dispose() = currentFunctionType <- previous }

    let withinClass classType =
        let previous = currentClassType
        currentClassType <- classType

        { new System.IDisposable with
            member x.Dispose() = currentClassType <- previous }

    let resolveVariable name =
        let depth =
            scopes.ToArray()
            |> Array.tryFindIndex (fun scope -> scope.ContainsKey(name.lexeme))

        match depth with
        | Some depth -> interpreter.resolve name depth
        | None -> ()

    let resolveFunc func funcType resolve =
        use _ = createScope ()
        use _ = withinFunction funcType

        func.paramList
        |> List.iter (fun param ->
            declare param
            define param)

        func.body |> List.iter resolve

    let exprVisitor =
        { new UnitExprVisitor() with
            override x.visitLiteral value = ()

            override x.visitUnary(operator, right) = x.visit (right)

            override x.visitBinary(left, operator, right) =
                x.visit left
                x.visit right


            override x.visitVariable name =
                match scopes.TryPeek() with
                | true, scope ->
                    match scope.TryGetValue name.lexeme with
                    | true, false -> lox.error name "Can't read local variable in its own initializer."
                    | _ -> ()
                | _ -> ()

                resolveVariable name

            override x.visitAssign(name, expr) =
                x.visit expr
                resolveVariable name

            override x.visitLogical left operator right =
                x.visit left
                x.visit right

            override x.visitCall callee args paren =
                x.visit callee
                args |> List.iter x.visit

            override x.visitGet callee name = x.visit callee

            override x.visitSet callee name value =
                x.visit value
                x.visit callee

            override x.visitThis keyword =
                if currentClassType = ClassType.NONE then
                    lox.error keyword "Can't use 'this' outside of a class."

                resolveVariable keyword



        }

    let resolveExpr = exprVisitor.visit

    let stmtVisitor =
        { new StmtVisitor() with
            override x.visitExpression expr = resolveExpr expr

            override x.visitPrint expr = resolveExpr expr

            override x.visitVarDeclar name expr =
                declare name
                expr |> Option.iter resolveExpr
                define name


            override x.visitBlock stmts =
                use _ = createScope ()
                stmts |> List.iter x.visit


            override x.visitIf condition thenPart elsePart =
                resolveExpr condition
                x.visit thenPart

                match elsePart with
                | Some elsePart -> x.visit elsePart
                | None -> ()

            override x.visitWhile condition body =
                resolveExpr condition
                x.visit body

            override x.visitFunDeclar func =
                declare func.name
                define func.name
                resolveFunc func FUNCTION x.visit

            override x.visitReturn keyword expr =
                if currentFunctionType = FunctionType.NONE then
                    lox.error keyword "Can't return from top-level code."

                match expr with
                | Some expr ->
                    if currentFunctionType = INITIALIZER then
                        lox.error keyword "Can't return a value from an initializer."

                    resolveExpr expr
                | None -> ()

            override x.visitClass name methods =
                declare name
                define name

                use _ = createScope ()
                use _ = withinClass CLASS
                defineByName "this"

                methods
                |> List.iter (fun func ->
                    let funcType = if func.name.lexeme = "init" then INITIALIZER else METHOD
                    resolveFunc func funcType x.visit) }

    member x.resolve(stmts: Stmt seq) = stmts |> Seq.iter stmtVisitor.visit
