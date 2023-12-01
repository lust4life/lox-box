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
    | SUBCLASS

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

    member x.resolve(expr: Expr) = expr.accept x |> ignore
    member x.resolve(stmt: Stmt) = stmt.accept x |> ignore
    member x.resolve(stmts: Stmt seq) = stmts |> Seq.iter x.resolve


    interface ExprVisitor<obj> with
        override x.visitLiteral value = ()

        override x.visitUnary operator right = x.resolve right

        override x.visitBinary left operator right =
            x.resolve left
            x.resolve right

        override x.visitVariable name =
            match scopes.TryPeek() with
            | true, scope ->
                match scope.TryGetValue name.lexeme with
                | true, false -> lox.error name "Can't read local variable in its own initializer."
                | _ -> ()
            | _ -> ()

            resolveVariable name

        override x.visitAssign name expr =
            x.resolve expr
            resolveVariable name

        override x.visitLogical left operator right =
            x.resolve left
            x.resolve right

        override x.visitCall callee args paren =
            x.resolve callee
            args |> List.iter x.resolve |> box

        override x.visitGet callee name = x.resolve callee

        override x.visitSet callee name value =
            x.resolve value
            x.resolve callee

        override x.visitThis keyword =
            if currentClassType = ClassType.NONE then
                lox.error keyword "Can't use 'this' outside of a class."

            resolveVariable keyword

        override x.visitSuper keyword method =
            match currentClassType with
            | ClassType.NONE -> lox.error keyword "Can't use 'super' outside of a class."
            | SUBCLASS -> ()
            | _ -> lox.error keyword "Can't use 'super' in a class with no superclass."

            resolveVariable keyword

        override x.visitGrouping expr = ()


    interface StmtVisitor<obj> with
        override x.visitExpression expr = x.resolve expr

        override x.visitPrint expr = x.resolve expr

        override x.visitVarDeclar name expr =
            declare name
            expr |> Option.iter x.resolve
            define name


        override x.visitBlock stmts =
            use _ = createScope ()
            x.resolve stmts


        override x.visitIf condition thenPart elsePart =
            x.resolve condition
            x.resolve thenPart

            match elsePart with
            | Some elsePart -> x.resolve elsePart
            | None -> ()

        override x.visitWhile condition body =
            x.resolve condition
            x.resolve body

        override x.visitFunDeclar func =
            declare func.name
            define func.name
            resolveFunc func FUNCTION x.resolve

        override x.visitReturn keyword expr =
            if currentFunctionType = FunctionType.NONE then
                lox.error keyword "Can't return from top-level code."

            match expr with
            | Some expr ->
                if currentFunctionType = INITIALIZER then
                    lox.error keyword "Can't return a value from an initializer."

                x.resolve expr
            | None -> ()

        override x.visitClass name methods superclass =
            declare name
            define name

            let superScope =
                superclass
                |> Option.map (fun superclass ->
                    if name.lexeme = superclass.lexeme then
                        lox.error superclass "A class can't inherit from itself."

                    resolveVariable superclass

                    let superScope = createScope ()
                    defineByName "super"

                    superScope)

            let thisScope = createScope ()

            let classType =
                match superclass with
                | Some _ -> SUBCLASS
                | None -> CLASS

            use _ = withinClass classType
            defineByName "this"

            methods
            |> List.iter (fun func ->
                let funcType = if func.name.lexeme = "init" then INITIALIZER else METHOD
                resolveFunc func funcType x.resolve)

            thisScope.Dispose()

            superScope |> Option.iter (fun x -> x.Dispose())

            ()
