namespace lox.ast

open lox.expr
open System
open lox.token

type AstPrinter() =

    let parenthesize name exprs visit =
        seq {
            yield $"({name}"

            for expr in exprs do
                let exprStr = visit expr
                yield $" {exprStr}"

            yield ")"
        }
        |> System.String.Concat

    let parenthesize2 name (parts: obj list) visit =
        seq {
            yield $"({name}"

            for part in parts do
                yield " "

                let eachPart =
                    match part with
                    | :? Expr as part -> visit part
                    | :? Token as part -> part.lexeme
                    | _ -> part.ToString()

                yield eachPart

            yield ")"
        }
        |> System.String.Concat

    let exprVisitor =
        { new ExprVisitor<string>() with
            override x.visitLiteral value =
                match value with
                | null -> "nil"
                | :? double as number when Double.IsInteger(number) -> sprintf "%.1f" number
                | _ -> value.ToString()

            override x.visitUnary(operator, right) =
                parenthesize operator.lexeme [ right ] x.visit

            override x.visitBinary(left, operator, right) =
                parenthesize operator.lexeme [ left; right ] x.visit

            override x.visitGrouping expr = parenthesize "group" [ expr ] x.visit
            override x.visitVariable name = name.lexeme

            override x.visitAssign(name, value) =
                parenthesize2 "=" [ name.lexeme; value ] x.visit


            override x.visitLogical left operator right =
                parenthesize operator.lexeme [ left; right ] x.visit

            override x.visitCall callee args paren =
                parenthesize2 "call" (callee :: args |> List.map box) x.visit

            override x.visitGet callee name =
                parenthesize2 "." [ callee; name.lexeme ] x.visit

            override x.visitSet callee name value =
                parenthesize2 "=" [ callee; name.lexeme; value ] x.visit

            override x.visitThis keyword = "this"

            override x.visitSuper keyword method =
                parenthesize2 "super" [ method ] x.visit

        }


    member x.print = exprVisitor.visit
