namespace lox.expr

open lox.token

type Expr =
    | Binary of left: Expr * operator: Token * right: Expr
    | Grouping of expression: Expr
    | Literal of value: obj
    | Unary of operator: Token * right: Expr
