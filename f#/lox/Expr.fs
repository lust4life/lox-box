namespace lox.expr

open lox.token

type Expr =
    | Binary of left: Expr * operator: Token * right: Expr
    | Grouping of expression: Expr
    | Literal of value: obj
    | Unary of operator: Token * right: Expr
    | Variable of Token
    | Assign of Token * Expr
    | Logical of left: Expr * operator: Token * right: Expr
    | Call of callee: Expr * args: Expr list * paren: Token

[<AbstractClass>]
type ExprVisitor<'R>() =
    member x.visit(expr: Expr) =
        match expr with
        | Binary(left, operator, right) -> x.visitBinary (left, operator, right)
        | Grouping(expression) -> x.visitGrouping (expression)
        | Literal value -> x.visitLiteral value
        | Unary(operator, right) -> x.visitUnary (operator, right)
        | Variable name -> x.visitVariable name
        | Assign(name, expr) -> x.visitAssign (name, expr)
        | Logical(left, operator, right) -> x.visitLogical left operator right
        | Call(callee, args, paren) -> x.visitCall callee args paren

    abstract visitLiteral: obj -> 'R
    abstract visitUnary: Token * Expr -> 'R
    abstract visitAssign: Token * Expr -> 'R
    abstract visitBinary: Expr * Token * Expr -> 'R
    abstract visitVariable: Token -> 'R
    abstract visitLogical: Expr -> Token -> Expr -> 'R
    abstract visitCall: Expr -> Expr list -> Token -> 'R

    abstract visitGrouping: Expr -> 'R
    default x.visitGrouping expr = x.visit expr
