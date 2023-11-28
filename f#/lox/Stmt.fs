namespace lox.stmt

open lox.expr
open lox.token

type Stmt =
    | Expression of Expr
    | Print of Expr
    | VarDeclar of Token * Expr option
    | Block of Stmt list
    | If of condition: Expr * thenPart: Stmt * elsePart: Stmt option
    | While of condition: Expr * body: Stmt
    | FunDeclar of name: Token * paramList: Token list * body: Stmt list
    | Return of keyword: Token * expr: Expr option

[<AbstractClass>]
type StmtVisitor() =
    member x.visit(stmt: Stmt) =
        match stmt with
        | Print expr -> x.visitPrint expr
        | Expression expr -> x.visitExpression expr
        | VarDeclar(name: Token, expr) -> x.visitVarDeclar name expr
        | Block stmts -> x.visitBlock stmts
        | If(condition, thenPart, elsePart) -> x.visitIf condition thenPart elsePart
        | While(condition, body) -> x.visitWhile condition body
        | FunDeclar(name, paramList, body) -> x.visitFunDeclar name paramList body
        | Return(keyword, expr) -> x.visitReturn keyword expr

    abstract visitPrint: Expr -> unit
    abstract visitExpression: Expr -> unit
    abstract visitVarDeclar: Token -> Expr option -> unit
    abstract visitBlock: Stmt list -> unit
    abstract visitIf: Expr -> Stmt -> Stmt option -> unit
    abstract visitWhile: Expr -> Stmt -> unit
    abstract visitFunDeclar: Token -> Token list -> Stmt list -> unit
    abstract visitReturn: Token -> Expr option -> unit
