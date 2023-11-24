namespace lox

open System.IO
open System
open lox.scanner
open lox.parser
open lox.interpreter
open Argu

type CliArguments =
    | [<EqualsAssignment>] Test of string
    | [<MainCommand; Last>] CodePath of path: string

    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Test _ -> "specify a test suite which you assign to dart"
            | CodePath _ -> "the file path"

module cli =
    open System.Text.RegularExpressions


    let (|RegEx|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some m.Groups else None

    let run source =
        let scanner = Scanner(source)
        // scanner.scanTokens () |> Seq.iter (fun tk -> printfn "%s" (tk.ToString()))
        let tks = scanner.scanTokens ()
        let parser = Parser(tks |> Seq.toList)
        let interpreter = new Interpreter()
        // parser.parse () |> printfn "%A"
        let parseRes = parser.parse () |> Seq.toList

        if not lox.hadError then
            parseRes |> interpreter.interpret

    let scan source =
        let scanner = Scanner(source)
        scanner.scanTokens () |> Seq.iter (fun tk -> printfn "%s" (tk.ToString()))

    let runFile testSuite (path: string) =
        let source = File.ReadAllText path

        match testSuite with
        | Some suiteName ->
            match suiteName with
            | RegEx "chap04" _ -> scan source
            | RegEx "chap06" _ -> scan source
            | RegEx "chap07" _ -> scan source
            | RegEx "chap08" _ -> scan source
            | _ -> failwithf "not support test %s" suiteName
        | None -> run source

        if lox.hadError then
            exit 65

        if lox.hadRuntimeError then
            exit 70

    let runPrompt () =
        let mutable keepGoing = true

        while keepGoing do
            printfn "> "

            match Console.ReadLine() with
            | null -> keepGoing <- false
            | line -> run line




    [<EntryPoint>]
    let main args =
        let parser = ArgumentParser.Create<CliArguments>()
        let argParsed = parser.Parse args
        let testSuite = argParsed.TryGetResult Test
        let code_path = argParsed.TryGetResult CodePath

        match code_path with
        | Some file -> runFile testSuite file
        | _ -> runPrompt ()

        0
// match args with
// | [| file |] ->
//     runFile file
//     0
// | [||] ->
//     runPrompt ()
//     0
// | _ ->
//     printfn "Usage: lox [script]"
//     64
