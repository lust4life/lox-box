namespace lox.ast

open lox.expr
open System

type AstPrinter() =


    let rec exprVisitor =
        { new ExprVisitor<string>() with


            override x.visitLiteral value =
                match value with
                | null -> "nil"
                | :? double as number when Double.IsInteger(number) -> sprintf "%.1f" number
                | _ -> value.ToString()

            override x.visitUnary(operator, right) = parenthesize operator.lexeme [ right ]

            override x.visitBinary(left, operator, right) =
                parenthesize operator.lexeme [ left; right ]

            override x.visitGrouping expr = parenthesize "group" [ expr ]
            override x.visitVariable name = name.lexeme
            override x.visitAssign(name, value) = failwith "not impl" }

    and parenthesize name exprs =
        seq {
            yield $"({name}"

            for expr in exprs do
                let exprStr = exprVisitor.visit expr
                yield $" {exprStr}"

            yield ")"
        }
        |> System.String.Concat

    member x.print = exprVisitor.visit
