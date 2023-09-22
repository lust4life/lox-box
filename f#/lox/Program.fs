namespace lox

open System.IO
open System




module cli =
    let run source =
        let scanner: Scanner = Scanner(source)
        scanner.scanTokens () |> Seq.iter (fun tk -> printfn "%s" (tk.ToString()))



    let runFile (file: string) = File.ReadAllText file |> run

    let runPrompt () =
        let mutable keepGoing = true

        while keepGoing do
            printfn "> "

            match Console.ReadLine() with
            | null -> keepGoing <- false
            | line -> run line


    [<EntryPoint>]
    let main args =
        match args with
        | [| file |] ->
            runFile file
            0
        | [||] ->
            runPrompt ()
            0
        | _ ->
            printfn "Usage: lox [script]"
            64
