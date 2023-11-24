namespace lox.stmt

open lox.expr
open lox.token

type Stmt =
    | Expression of Expr
    | Print of Expr
    | VarDeclar of Token * Expr option


[<AbstractClass>]
type StmtVisitor() =
    member x.visit(stmt: Stmt) =
        match stmt with
        | Print expr -> x.visitPrint expr
        | Expression expr -> x.visitExpression expr
        | VarDeclar(name: Token, expr) -> x.visitVarDeclar name expr

    abstract visitPrint: Expr -> unit

    abstract visitExpression: Expr -> unit

    abstract visitVarDeclar: Token -> Expr option -> unit
