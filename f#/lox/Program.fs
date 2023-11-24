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
    open lox.ast


    let (|RegEx|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some m.Groups else None

    let run source =
        let scanner = Scanner(source)
        let parser = Parser(scanner.scanTokens () |> Seq.toList)
        let parseRes = parser.parse () |> Seq.toList

        if not lox.hadError then
            let interpreter = new Interpreter()
            parseRes |> interpreter.interpret

    let testChap04 source =
        let scanner = Scanner(source)
        scanner.scanTokens () |> Seq.iter (fun tk -> printfn "%s" (tk.ToString()))

    let testChap06 source =
        let scanner = Scanner(source)
        let parser = Parser(scanner.scanTokens () |> Seq.toList)
        let expr = parser.parseExpr ()
        let astPrinter = AstPrinter()
        printfn "%s" (astPrinter.print expr)

    let testChap07 source =
        let scanner = Scanner(source)
        let parser = Parser(scanner.scanTokens () |> Seq.toList)
        let expr = parser.parseExpr ()
        let interpreter = Interpreter()
        interpreter.evaluateExprAndPrint expr

    let runFile testSuite (path: string) =
        let source = File.ReadAllText path

        match testSuite with
        | Some suiteName ->
            match suiteName with
            | RegEx "chap04" _ -> testChap04 source
            | RegEx "chap06" _ -> testChap06 source
            | RegEx "chap07" _ -> testChap07 source
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
