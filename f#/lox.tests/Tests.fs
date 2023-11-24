namespace lox.tests

open Expecto

module tests =

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "universe exists (╭ರᴥ•́)"
              <| fun _ ->
                  let subject = true
                  Expect.isTrue subject "I compute, therefore I am."

              ]
