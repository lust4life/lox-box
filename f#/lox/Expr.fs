namespace rec lox.expr

open lox.token

type Expr =
    abstract accept: ExprVisitor<'R> -> 'R

type ExprVisitor<'R> =
    abstract visitLiteral: obj -> 'R
    abstract visitUnary: Token -> Expr -> 'R
    abstract visitAssign: Token -> Expr -> 'R
    abstract visitBinary: Expr -> Token -> Expr -> 'R
    abstract visitVariable: Token -> 'R
    abstract visitLogical: Expr -> Token -> Expr -> 'R
    abstract visitCall: Expr -> Expr list -> Token -> 'R
    abstract visitGet: Expr -> Token -> 'R
    abstract visitSet: Expr -> Token -> Expr -> 'R
    abstract visitThis: Token -> 'R
    abstract visitSuper: Token -> Token -> 'R
    abstract visitGrouping: Expr -> 'R

type Binary(left: Expr, operator: Token, right: Expr) =
    interface Expr with
        member x.accept visitor = visitor.visitBinary left operator right

type Grouping(expr: Expr) =
    interface Expr with
        member x.accept visitor = visitor.visitGrouping expr

type Literal(value: obj) =
    interface Expr with
        member x.accept visitor = visitor.visitLiteral value

type Unary(operator: Token, right: Expr) =
    interface Expr with
        member x.accept visitor = visitor.visitUnary operator right

type Variable(name: Token) =
    member x.name = name

    interface Expr with
        member x.accept visitor = visitor.visitVariable name

type Assign(name: Token, expr: Expr) =

    interface Expr with
        member x.accept visitor = visitor.visitAssign name expr

type Logical(left: Expr, operator: Token, right: Expr) =
    interface Expr with
        member x.accept visitor =
            visitor.visitLogical left operator right

type Call(callee: Expr, args: Expr list, paren: Token) =
    interface Expr with
        member x.accept visitor = visitor.visitCall callee args paren

type Get(callee: Expr, name: Token) =
    member x.callee = callee
    member x.name = name

    interface Expr with
        member x.accept visitor = visitor.visitGet callee name

type Set(callee: Expr, name: Token, value: Expr) =
    interface Expr with
        member x.accept visitor = visitor.visitSet callee name value

type This(keyword: Token) =
    interface Expr with
        member x.accept visitor = visitor.visitThis keyword

type Super(keyword: Token, method: Token) =
    interface Expr with
        member x.accept visitor = visitor.visitSuper keyword method
