use crate::scanner::{Scanner, TokenType};

pub struct Compiler;

impl Compiler {
    pub fn compile(source: &str) {
        let mut scanner = Scanner::new(source);
        let mut line = 0;
        loop {
            let tk = scanner.scan_token();
            if tk.line != line {
                line = tk.line;
                print!("   | ");
            } else {
                print!("{line:4} ");
            }
            println!("{:?} '{}'", tk.token_type, tk.lexeme);

            if tk.token_type == TokenType::TokenEOF {
                break;
            }
        }
    }
}
