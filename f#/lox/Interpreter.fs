namespace lox.interpreter

open lox
open lox.expr
open lox.token
open lox.stmt
open lox.env

type Interpreter() =
    let env = Environment()


    let castTruthy (value: obj) =
        match value with
        | null -> false
        | :? bool as boolVal -> boolVal
        | _ -> true

    let isEqual left right =
        match left, right with
        | null, null -> true
        | null, _ -> false
        | _ -> left = right

    let exprVisitor =
        { new ExprVisitor<obj>() with
            override x.visitLiteral value = value

            override x.visitUnary(operator, right) =
                let rightValue = x.visit (right)


                match operator.tokenType with
                | MINUS ->
                    match rightValue with
                    | :? double as rn -> -rn
                    | _ -> raise (RuntimeError(operator, "Operand must be a number."))
                | BANG -> not (castTruthy rightValue)
                | x -> failwithf "not support %A" x

            override x.visitBinary(left, operator, right) =
                let leftVal = x.visit (left)
                let rightVal = x.visit (right)

                let doWithNumberCast operatorFunc =
                    match leftVal, rightVal with
                    | :? double as ln, (:? double as rn) -> operatorFunc ln rn |> box
                    | _ -> raise (RuntimeError(operator, "Operands must be numbers."))

                let doPlus () =
                    match leftVal, rightVal with
                    | :? string as ls, (:? string as rs) -> ls + rs |> box
                    | :? double as ln, (:? double as rn) -> ln + rn |> box
                    | _ -> raise (RuntimeError(operator, "Operands must be two numbers or two strings."))

                match operator.tokenType with
                | BANG_EQUAL -> not (isEqual leftVal rightVal)
                | EQUAL_EQUAL -> isEqual leftVal rightVal
                | GREATER -> doWithNumberCast (>)
                | GREATER_EQUAL -> doWithNumberCast (>=)
                | LESS -> doWithNumberCast (<)
                | LESS_EQUAL -> doWithNumberCast (<=)
                | MINUS -> doWithNumberCast (-)
                | PLUS -> doPlus ()
                | SLASH -> doWithNumberCast (/)
                | STAR -> doWithNumberCast (*)
                | x -> failwithf "not support %A" x

            override x.visitVariable name = env.get name

            override x.visitAssign(name, expr) =
                let value = x.visit expr
                env.assign name value
                value

        }



    let evaluate = exprVisitor.visit

    let stringify (value: obj) =
        match value with
        | null -> "nil"
        | :? double as number ->
            let numberStr = string number

            if numberStr.EndsWith(".0") then
                numberStr.Substring(0, numberStr.Length - 2)
            else
                numberStr
        | _ -> value.ToString()

    let stmtVisitor =
        { new StmtVisitor() with
            override x.visitExpression expr = evaluate expr |> ignore

            override x.visitPrint expr =
                evaluate expr |> stringify |> printfn "%s"

            override x.visitVarDeclar name expr =
                let initializer = expr |> Option.map evaluate
                let value = initializer |> Option.defaultValue null
                env.define name value }

    let execute = stmtVisitor.visit

    member x.interpret(stmts: Stmt seq) =
        try
            stmts |> Seq.iter execute
        with :? RuntimeError as error ->
            lox.runtimeError error

    member x.evaluateExprAndPrint = stmtVisitor.visitPrint
