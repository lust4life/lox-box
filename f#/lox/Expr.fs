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
    | Get of callee: Expr * name: Token
    | Set of callee: Expr * name: Token * value: Expr
    | This of keyword: Token

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
        | Get(callee, name) -> x.visitGet callee name
        | Set(callee, name, value) -> x.visitSet callee name value
        | This(keyword) -> x.visitThis keyword

    abstract visitLiteral: obj -> 'R
    abstract visitUnary: Token * Expr -> 'R
    abstract visitAssign: Token * Expr -> 'R
    abstract visitBinary: Expr * Token * Expr -> 'R
    abstract visitVariable: Token -> 'R
    abstract visitLogical: Expr -> Token -> Expr -> 'R
    abstract visitCall: Expr -> Expr list -> Token -> 'R
    abstract visitGet: Expr -> Token -> 'R
    abstract visitSet: Expr -> Token -> Expr -> 'R
    abstract visitThis: Token -> 'R

    abstract visitGrouping: Expr -> 'R
    default x.visitGrouping expr = x.visit expr

/// <summary>unit is not same as c#'s void, a bit tricky here</summary>
[<AbstractClass>]
type UnitExprVisitor() =
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
        | Get(callee, name) -> x.visitGet callee name
        | Set(callee, name, value) -> x.visitSet callee name value
        | This(keyword) -> x.visitThis keyword

    abstract visitLiteral: obj -> unit
    abstract visitUnary: Token * Expr -> unit
    abstract visitAssign: Token * Expr -> unit
    abstract visitBinary: Expr * Token * Expr -> unit
    abstract visitVariable: Token -> unit
    abstract visitLogical: Expr -> Token -> Expr -> unit
    abstract visitCall: Expr -> Expr list -> Token -> unit
    abstract visitGet: Expr -> Token -> unit
    abstract visitSet: Expr -> Token -> Expr -> unit
    abstract visitThis: Token -> unit

    abstract visitGrouping: Expr -> unit
    default x.visitGrouping expr = x.visit expr
