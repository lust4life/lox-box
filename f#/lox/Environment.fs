namespace lox.env

open System.Collections.Generic
open lox
open lox.token

type Environment(enclosing: Environment option) =
    let infos = Dictionary<string, obj>()

    new() = Environment(None)

    member x.define (name: Token) value = x.defineByName name.lexeme value

    member x.defineByName (name: string) value = infos[name] <- value

    member x.get(name: Token) =
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
