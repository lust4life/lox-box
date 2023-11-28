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

type Resolver(interpreter: Interpreter) =

    let scopes = Stack<Dictionary<string, bool>>()
    let mutable currentFunctionType = NONE

    let declare (name: Token) =
        match scopes.TryPeek() with
        | true, scope ->
            if scope.ContainsKey name.lexeme then
                lox.error name "Already a variable with this name in this scope."

            scope[name.lexeme] <- false
        | _ -> ()

    let define (name: Token) =
        match scopes.TryPeek() with
        | true, scope -> scope[name.lexeme] <- true
        | _ -> ()

    let createScope () =
        let scope = Dictionary<_, _>()
        scopes.Push scope

        { new System.IDisposable with
            member x.Dispose() = scopes.Pop() |> ignore }

    let withinFunction () =
        let previous = currentFunctionType
        currentFunctionType <- FUNCTION

        { new System.IDisposable with
            member x.Dispose() = currentFunctionType <- previous }

    let resolveVariable name =
        let depth =
            scopes.ToArray()
            |> Array.tryFindIndex (fun scope -> scope.ContainsKey(name.lexeme))

        match depth with
        | Some depth -> interpreter.resolve name depth
        | None -> ()

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

        }

    let resolveExpr = exprVisitor.visit

    let stmtVisitor =
        {

          new StmtVisitor() with
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

              override x.visitFunDeclar name paramList body =
                  declare name
                  define name

                  use _ = createScope ()
                  use _ = withinFunction ()

                  paramList
                  |> List.iter (fun param ->
                      declare param
                      define param)

                  body |> List.iter x.visit

              override x.visitReturn keyword expr =
                  if currentFunctionType = NONE then
                      lox.error keyword "Can't return from top-level code."

                  match expr with
                  | Some expr -> resolveExpr expr
                  | None -> ()

        }

    member x.resolve(stmts: Stmt seq) = stmts |> Seq.iter stmtVisitor.visit
