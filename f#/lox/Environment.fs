namespace lox.env

open System.Collections.Generic
open lox
open lox.token

type Environment(enclosing: Environment option) =
    let infos = Dictionary<string, obj>()

    let rec ancestor depth (env: Environment) =
        if depth > 0 then
            ancestor (depth - 1) (env.Enclosing |> Option.get)
        else
            env

    new() = Environment(None)

    member x.define (tk: Token) value = x.defineByName tk.lexeme value

    member x.defineByName (name: string) value = infos[name] <- value

    member x.getAt (depth) (tk: Token) = x.getAtByName depth tk.lexeme

    member x.getAtByName (depth) (name) =
        let theOne = ancestor depth x
        theOne.rawGet name

    member x.get name =
        let key = name.lexeme

        match infos.TryGetValue key with
        | true, value -> value
        | _ ->
            match enclosing with
            | Some enclosing -> enclosing.get name
            | None -> raise (RuntimeError(name, $"Undefined variable '{key}'."))


    member x.assign name value =
        let key = name.lexeme

        match infos.ContainsKey key with
        | true -> infos[key] <- value
        | _ ->
            match enclosing with
            | Some enclosing -> enclosing.assign name value
            | None -> raise (RuntimeError(name, $"Undefined variable '{key}'."))

    member x.assignAt depth (name: Token) value =
        let theOne = ancestor depth x
        theOne.rawAssign name.lexeme value


    member x.rawGet name = infos[name]
    member x.rawAssign name value = infos[name] <- value
    member x.Enclosing = enclosing
