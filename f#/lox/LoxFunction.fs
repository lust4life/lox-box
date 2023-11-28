namespace lox.func

open lox.token
open lox.stmt

type LoxFunction(name: Token, paramList: Token list, body: Stmt list) =
    member x.Call(args: obj list) : obj = ()
