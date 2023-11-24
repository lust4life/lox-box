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
        parser.parse () |> interpret.interpret

    [<Tests>]
    let tests =
        testList
            "parser"
            [ testCase "xx"
              <| fun _ ->
                  parse
                  <| """
                    // Tests that we correctly track the line info across multiline strings.
                    var a = "1
                    2
                    3
                    ";
                    
                    err; // // expect runtime error: Undefined variable 'err'.
                    """ ]
