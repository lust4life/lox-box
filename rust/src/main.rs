use std::{env, fs, io::Write, process::exit};

use lox_box::vm::VM;

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<String>>();
    match args.len() {
        1 => {
            repl()?;
        }
        2 => {
            run_file(&args[1])?;
        }
        _ => {
            println!("Usage: clox [path]");
            exit(64);
        }
    };

    return Ok(());
}

fn run_file(path: &str) -> std::io::Result<()> {
    let source = fs::read_to_string(path)?;

    let mut vm = VM::new();
    vm.interpret(&source);

    return Ok(());
}

fn repl() -> std::io::Result<()> {
    let mut vm = VM::new();
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    loop {
        print!("> ");
        stdout.flush()?;

        let mut line = String::new();
        let len = stdin.read_line(&mut line)?;
        if len == 0 {
            break;
        }

        vm.interpret(&line);
    }

    return Ok(());
}
