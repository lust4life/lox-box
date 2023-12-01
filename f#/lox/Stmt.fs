namespace rec lox.stmt

open lox.expr
open lox.token

type Stmt =
    abstract accept: StmtVisitor<'R> -> 'R

type StmtVisitor<'R> =
    abstract visitPrint: Expr -> 'R
    abstract visitExpression: Expr -> 'R
    abstract visitVarDeclar: Token -> Expr option -> 'R
    abstract visitBlock: Stmt list -> 'R
    abstract visitIf: Expr -> Stmt -> Stmt option -> 'R
    abstract visitWhile: Expr -> Stmt -> 'R
    abstract visitFunDeclar: Fun -> 'R
    abstract visitReturn: Token -> Expr option -> 'R
    abstract visitClass: Token -> Fun list -> Token option -> 'R

type Expression(expr: Expr) =
    interface Stmt with
        member x.accept visitor = visitor.visitExpression expr

type Print(expr: Expr) =
    interface Stmt with
        member x.accept visitor = visitor.visitPrint expr

type VarDeclar(name: Token, expr: Expr option) =
    interface Stmt with
        member x.accept visitor = visitor.visitVarDeclar name expr

type Block(stmts: Stmt list) =
    member x.stmts = stmts

    interface Stmt with
        member x.accept visitor = visitor.visitBlock stmts

type If(condition: Expr, thenPart: Stmt, elsePart: Stmt option) =
    interface Stmt with
        member x.accept visitor =
            visitor.visitIf condition thenPart elsePart

type While(condition: Expr, body: Stmt) =
    interface Stmt with
        member x.accept visitor = visitor.visitWhile condition body

type FunDeclar(func: Fun) =
    interface Stmt with
        member x.accept visitor = visitor.visitFunDeclar func

type Return(keyword: Token, expr: Expr option) =
    interface Stmt with
        member x.accept visitor = visitor.visitReturn keyword expr

type Class(name: Token, methods: Fun list, superclass: Token option) =
    interface Stmt with
        member x.accept visitor =
            visitor.visitClass name methods superclass

type Fun =
    { name: Token
      paramList: Token list
      body: Stmt list }
