namespace lox

type Scanner(source: string) =

    member x.scanTokens() : string seq = [ source ]
