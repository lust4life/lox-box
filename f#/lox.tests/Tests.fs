namespace lox.tests

open Expecto

module tests =
    open lox.parser
    open lox.scanner

    let parse code =
        let scanner = Scanner(code)
        let parser = Parser(scanner.scanTokens () |> Seq.toList)
        parser.parseExpr ()

    [<Tests>]
    let tests =
        testList
            "parser"
            [ testCase "xx"
              <| fun _ ->
                  let stmts =
                      parse
                      <| """
                         (1+2)
                         """

                  stmts |> printfn "%A" ]
