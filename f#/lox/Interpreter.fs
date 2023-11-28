namespace lox.interpreter

open lox
open lox.expr
open lox.token
open lox.stmt
open lox.env
open System.Collections.Generic

exception ReturnError of obj

type Interpreter() as interpreter =
    let globalEnv = Environment()
    let mutable localEnv = globalEnv
    let resolvedState = Dictionary<Token, int>()

    let initializeNativeFun () =
        globalEnv.defineByName
            "clock"
            {

              new System.Object() with
                  member x.ToString() = "<native fn>"
              interface LoxCallable with
                  member x.call interpreter args paren =
                      System.DateTimeOffset.UtcNow.ToUnixTimeSeconds()

            }

    do initializeNativeFun ()

    let lookUpVariable name =
        match resolvedState.TryGetValue name with
        | true, depth -> localEnv.getAt depth name
        | _ -> globalEnv.get name

    let assignVariable name value =
        match resolvedState.TryGetValue name with
        | true, depth -> localEnv.assignAt depth name value
        | _ -> globalEnv.assign name value

    let castTruthy (value: obj) =
        match value with
        | null -> false
        | :? bool as boolVal -> boolVal
        | _ -> true

    let isEqual left right =
        match left, right with
        | null, null -> true
        | null, _ -> false
        | _ -> left = right

    let exprVisitor =
        { new ExprVisitor<obj>() with
            override x.visitLiteral value = value

            override x.visitUnary(operator, right) =
                let rightValue = x.visit (right)


                match operator.tokenType with
                | MINUS ->
                    match rightValue with
                    | :? double as rn -> -rn
                    | _ -> raise (RuntimeError(operator, "Operand must be a number."))
                | BANG -> not (castTruthy rightValue)
                | x -> failwithf "not support %A" x

            override x.visitBinary(left, operator, right) =
                let leftVal = x.visit (left)
                let rightVal = x.visit (right)

                let doWithNumberCast operatorFunc =
                    match leftVal, rightVal with
                    | :? double as ln, (:? double as rn) -> operatorFunc ln rn |> box
                    | _ -> raise (RuntimeError(operator, "Operands must be numbers."))

                let doPlus () =
                    match leftVal, rightVal with
                    | :? string as ls, (:? string as rs) -> ls + rs |> box
                    | :? double as ln, (:? double as rn) -> ln + rn |> box
                    | _ -> raise (RuntimeError(operator, "Operands must be two numbers or two strings."))

                match operator.tokenType with
                | BANG_EQUAL -> not (isEqual leftVal rightVal)
                | EQUAL_EQUAL -> isEqual leftVal rightVal
                | GREATER -> doWithNumberCast (>)
                | GREATER_EQUAL -> doWithNumberCast (>=)
                | LESS -> doWithNumberCast (<)
                | LESS_EQUAL -> doWithNumberCast (<=)
                | MINUS -> doWithNumberCast (-)
                | PLUS -> doPlus ()
                | SLASH -> doWithNumberCast (/)
                | STAR -> doWithNumberCast (*)
                | x -> failwithf "not support %A" x

            override x.visitVariable name = lookUpVariable name

            override x.visitAssign(name, expr) =
                let value = x.visit expr
                assignVariable name value
                value

            override x.visitLogical left operator right =
                let leftValue = x.visit left

                match operator.tokenType with
                | OR -> if castTruthy leftValue then leftValue else x.visit right
                | AND -> if castTruthy leftValue then x.visit right else leftValue
                | _ -> failwith "should not happen"

            override x.visitCall callee args paren =
                let callee = x.visit callee
                let args = args |> List.map x.visit

                match callee with
                | :? LoxCallable as loxCallable -> loxCallable.call interpreter args paren
                | _ -> raise (RuntimeError(paren, "Can only call functions and classes."))

        }

    let evaluate = exprVisitor.visit

    let stringify (value: obj) =
        match value with
        | null -> "nil"
        | :? double as number ->
            let numberStr = string number

            if numberStr.EndsWith(".0") then
                numberStr.Substring(0, numberStr.Length - 2)
            else
                numberStr
        | :? bool as value -> value.ToString().ToLower()
        | _ -> value.ToString()

    let stmtVisitor =
        {

          new StmtVisitor() with
              override x.visitExpression expr = evaluate expr |> ignore

              override x.visitPrint expr =
                  evaluate expr |> stringify |> printfn "%s"

              override x.visitVarDeclar name expr =
                  let initializer = expr |> Option.map evaluate
                  let value = initializer |> Option.defaultValue null
                  localEnv.define name value

              override x.visitBlock stmts =
                  let previousEnv = localEnv

                  try
                      localEnv <- Environment(Some localEnv)

                      for stmt in stmts do
                          x.visit stmt
                  finally
                      localEnv <- previousEnv

              override x.visitIf condition thenPart elsePart =
                  let conditionValue = evaluate condition |> castTruthy

                  if conditionValue then
                      x.visit thenPart
                  else
                      match elsePart with
                      | Some elsePart -> x.visit elsePart
                      | None -> ()

              override x.visitWhile condition body =
                  while evaluate condition |> castTruthy do
                      x.visit body

              override x.visitFunDeclar name paramList body =
                  // cause we are using static scope, so we capture the environment when define the function
                  let loxFunction = LoxFunction(name, paramList, body, localEnv)
                  localEnv.define name loxFunction

              override x.visitReturn keyword expr =
                  let res = expr |> Option.map evaluate |> Option.toObj
                  raise (ReturnError(res))

        }

    let execute = stmtVisitor.visit

    member x.interpret(stmts: Stmt seq) =
        try
            stmts |> Seq.iter execute
        with :? RuntimeError as error ->
            lox.runtimeError error

    member x.evaluateExprAndPrint = stmtVisitor.visitPrint

    member x.executeBlock (stmts: Stmt seq) (funEnv: Environment) =
        let previous = localEnv

        try
            localEnv <- funEnv
            x.interpret stmts
        finally
            localEnv <- previous

    member x.resolve name depth = resolvedState[name] <- depth

and LoxFunction(name: Token, paramList: Token list, body: Stmt list, closure: Environment) =

    interface LoxCallable with
        member x.call (interpreter: Interpreter) (args: obj list) (paren) : obj =
            if (args.Length <> paramList.Length) then
                raise (RuntimeError(paren, $"Expected {paramList.Length} arguments but got {args.Length}."))


            try
                let funEnv = Environment(Some closure)

                args
                |> List.zip paramList
                |> List.iter (fun (name, value) -> funEnv.define name value)

                interpreter.executeBlock body funEnv
            with ReturnError value ->
                value

    override x.ToString() = $"<fn {name.lexeme}>"

and LoxCallable =
    abstract call: Interpreter -> obj list -> Token -> obj
