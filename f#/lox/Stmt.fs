namespace lox.stmt

open lox.expr

type Stmt =
    | Expression of Expr
    | Print of Expr


[<AbstractClass>]
type StmtVisitor() =
    member x.visit(stmt: Stmt) =
        match stmt with
        | Print expr -> x.visitPrint expr
        | Expression expr -> x.visitExpression expr

    abstract visitPrint: Expr -> unit

    abstract visitExpression: Expr -> unit
