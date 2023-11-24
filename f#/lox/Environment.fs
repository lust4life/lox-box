namespace lox.env

open System.Collections.Generic
open lox
open lox.token

type Environment() =
    let infos = Dictionary<string, obj>()

    member x.define (name: Token) value = infos[name.lexeme] <- value

    member x.get(name: Token) =
        let key = name.lexeme

        match infos.TryGetValue key with
        | true, value -> value
        | _ -> raise (RuntimeError(name, $"Undefined variable '{key}'."))
