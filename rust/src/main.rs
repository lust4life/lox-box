use std::{env, fs, io::Write, process::exit};

use lox_box::vm::{self, InterpretResult};

fn main() -> InterpretResult {
    let args = env::args().collect::<Vec<String>>();
    match args.len() {
        1 => {
            return repl();
        }
        2 => {
            return run_file(&args[1]);
        }
        _ => {
            println!("Usage: clox [path]");
            exit(64);
        }
    }
}

fn run_file(path: &str) -> InterpretResult {
    let source = fs::read_to_string(path)?;
    return vm::interpret(&source);
}

fn repl() -> InterpretResult {
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

        vm::interpret(&line);
    }

    return InterpretResult::InterpretOk;
}
