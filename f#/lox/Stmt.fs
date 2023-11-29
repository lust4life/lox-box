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
    | FunDeclar of Fun
    | Return of keyword: Token * expr: Expr option
    | Class of name: Token * methods: Fun list

and Fun =
    { name: Token
      paramList: Token list
      body: Stmt list }


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
        | FunDeclar func -> x.visitFunDeclar func
        | Return(keyword, expr) -> x.visitReturn keyword expr
        | Class(name, methods) -> x.visitClass name methods

    abstract visitPrint: Expr -> unit
    abstract visitExpression: Expr -> unit
    abstract visitVarDeclar: Token -> Expr option -> unit
    abstract visitBlock: Stmt list -> unit
    abstract visitIf: Expr -> Stmt -> Stmt option -> unit
    abstract visitWhile: Expr -> Stmt -> unit
    abstract visitFunDeclar: Fun -> unit
    abstract visitReturn: Token -> Expr option -> unit
    abstract visitClass: Token -> Fun list -> unit
