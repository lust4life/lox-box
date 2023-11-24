namespace lox.tests

open Expecto

module tests =
    open lox.parser
    open lox.scanner
    open lox.interpreter

    let parse code =
        let scanner = Scanner(code)
        let parser = Parser(scanner.scanTokens () |> Seq.toList)
        let interpret = Interpreter()
        parser.parseExpr () |> interpret.evaluateExprAndPrint

    [<Tests>]
    let tests =
        testList
            "parser"
            [ testCase "xx"
              <| fun _ ->
                  parse
                  <| """
                    -1
                    """ ]
