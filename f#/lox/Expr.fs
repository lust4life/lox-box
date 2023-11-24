namespace lox.expr

open lox.token

type Expr =
    | Binary of left: Expr * operator: Token * right: Expr
    | Grouping of expression: Expr
    | Literal of value: obj
    | Unary of operator: Token * right: Expr
    | Variable of Token

[<AbstractClass>]
type ExprVisitor<'R>() =
    member x.visit(expr: Expr) =
        match expr with
        | Binary(left, operator, right) -> x.visitBinary (left, operator, right)
        | Grouping(expression) -> x.visitGrouping (expression)
        | Literal value -> x.visitLiteral value
        | Unary(operator, right) -> x.visitUnary (operator, right)
        | Variable(_) -> failwith "Not Implemented for visitVariable"

    abstract visitLiteral: obj -> 'R
    abstract visitUnary: Token * Expr -> 'R
    abstract visitBinary: Expr * Token * Expr -> 'R

    abstract visitGrouping: Expr -> 'R
    default x.visitGrouping expr = x.visit expr
