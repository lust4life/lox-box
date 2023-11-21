namespace lox.interpreter

open lox.expr
open lox.token

type Interpreter() =
    inherit ExprVisitor<obj>()

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

    override x.visitLiteral value = value

    override x.visitUnary(operator, right) =
        let rightValue = x.visit (right)

        match operator.tokenType with
        | MINUS -> -(downcast rightValue)
        | BANG -> not (castTruthy rightValue)
        | x -> failwithf "not support %A" x

    override x.visitBinary(left, operator, right) =
        let leftVal = x.visit (left)
        let rightVal = x.visit (right)

        let doWithCast operator =
            operator (leftVal :?> double) (rightVal :?> double) |> box

        let doPlus () =
            match leftVal, rightVal with
            | :? string as ls, (:? string as rs) -> ls + rs |> box
            | :? double as ln, (:? double as rn) -> ln + rn |> box
            | _ -> failwithf "not support + with %A %A" leftVal rightVal

        match operator.tokenType with
        | BANG_EQUAL -> not (isEqual leftVal rightVal)
        | EQUAL_EQUAL -> isEqual leftVal rightVal
        | GREATER -> doWithCast (>)
        | GREATER_EQUAL -> doWithCast (>=)
        | LESS -> doWithCast (<)
        | LESS_EQUAL -> doWithCast (<=)
        | MINUS -> doWithCast (-)
        | PLUS -> doPlus ()
        | SLASH -> doWithCast (/)
        | STAR -> doWithCast (*)
        | x -> failwithf "not support %A" x


module interpreter =

    let evaluate (expr: Expr) =
        let interpreter = Interpreter()
        interpreter.visit (expr)
