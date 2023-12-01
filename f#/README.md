# run dart tests

- build interpreter first by using `dotnet build` under `f#/lox`
- go to `craftinginterpreters` folder
- run `dart ./tool/bin/test.dart chap13_inheritance --interpreter ../f#/lox/bin/Debug/net7.0/lox`
  - `chap13_inheritance` can be suite name defined by `java("chap04_scanning"` in `tool/bin/test.dart` file
  - cause the early chapters are `No interpreter yet`, so for these chapters (see below), we need add one more argument (`-a --test=chap04`) for the cmd
    - chap04_scanning : `dart ./tool/bin/test.dart chap04_scanning --interpreter ../f#/lox/bin/Debug/net7.0/lox -a --test=chap04`
    - chap06_parsing : `dart ./tool/bin/test.dart chap06_parsing --interpreter ../f#/lox/bin/Debug/net7.0/lox -a --test=chap06`
    - chap07_evaluating : `dart ./tool/bin/test.dart chap07_evaluating --interpreter ../f#/lox/bin/Debug/net7.0/lox -a --test=chap07`
