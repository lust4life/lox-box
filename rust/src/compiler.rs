use crate::{
    chunk::Chunk,
    op::OpCode::{self, *},
    scanner::{
        Scanner, Token,
        TokenType::{self, *},
        TOKEN_PLACEHOLDER,
    },
};

#[derive(PartialEq, PartialOrd, Debug)]
enum Precedence {
    PrecNone,
    PrecAssignment, // =
    PrecOr,         // or
    PrecAnd,        // and
    PrecEquality,   // == !=
    PrecComparison, // < > <= >=
    PrecTerm,       // + -
    PrecFactor,     // * /
    PrecUnary,      // ! -
    PrecCall,       // . ()
    PrecPrimary,
}

impl Precedence {
    fn next(&self) -> Self {
        match self {
            PrecNone => PrecAssignment,
            PrecAssignment => PrecOr,
            PrecOr => PrecAnd,
            PrecAnd => PrecEquality,
            PrecEquality => PrecComparison,
            PrecComparison => PrecTerm,
            PrecTerm => PrecFactor,
            PrecFactor => PrecUnary,
            PrecUnary => PrecCall,
            PrecCall => PrecPrimary,
            PrecPrimary => todo!(),
        }
    }
}

use Precedence::*;

type ParseFun<'code, 'tk> = fn(&mut Parser<'code, 'tk>);

#[derive(Debug)]
struct ParseRule<'code, 'tk> {
    prefix: Option<ParseFun<'code, 'tk>>,
    infix: Option<ParseFun<'code, 'tk>>,
    precedence: Precedence,
}

const RULE_LENGTH: usize = TokenEOF as usize + 1usize;

struct Parser<'code: 'tk, 'tk> {
    chunk: Chunk,
    scanner: Scanner<'code>,
    had_error: bool,
    panic_mode: bool,
    current: Token<'tk>,
    next: Token<'tk>,
    rules: [Option<ParseRule<'code, 'tk>>; RULE_LENGTH],
}

impl<'code, 'tk> Parser<'code, 'tk> {
    fn new(source: &'code str) -> Self {
        let rules = init_rules();
        let scanner = Scanner::new(source);
        Self {
            chunk: Chunk::new(),
            scanner,
            had_error: false,
            panic_mode: false,
            current: TOKEN_PLACEHOLDER,
            next: TOKEN_PLACEHOLDER,
            rules: rules,
        }
    }

    fn compile(mut self) -> Option<Chunk> {
        self.advance();
        self.expression();
        self.consume(TokenEOF, "Expect end of expression.");

        if self.had_error {
            return None;
        }

        self.emit_byte(OpReturn);
        return Some(self.chunk);
    }

    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        self.chunk.write_chunk(byte.into(), self.current.line);
    }

    fn emit_bytes(&mut self, op_code: OpCode, byte2: u8) {
        self.emit_byte(op_code);
        self.emit_byte(byte2);
    }

    fn error_at(&mut self, tk: Token, msg: &str) {
        if self.panic_mode {
            return;
        }

        self.panic_mode = true;
        self.had_error = true;

        eprint!("[line {}] Error", tk.line);
        match tk.token_type {
            TokenError => (),
            TokenEOF => eprint!(" at end"),
            _ => eprint!(" at '{}'", tk.lexeme),
        }
        eprintln!(": {msg}");
    }

    fn consume(&mut self, match_type: TokenType, msg: &str) {
        let tk = self.next;
        if tk.token_type == match_type {
            self.advance();
            return;
        }

        self.error_at(tk, msg);
    }

    fn advance(&mut self) {
        self.current = self.next;

        loop {
            let tk = self.scanner.scan_token();
            if !tk.is_error_tk() {
                self.next = tk;
                break;
            }

            self.error_at(tk, tk.lexeme);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(PrecAssignment);
    }

    fn number(&mut self) {
        let tk = self.current;
        let number = tk.lexeme.parse::<f64>().unwrap();
        let idx = self.chunk.add_constant(number);
        if idx > u8::MAX.into() {
            self.error_at(tk, "Too many constants in one chunk.");
        }

        self.emit_bytes(OpConstant, idx as _)
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenRightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self) {
        let operation_type = self.current.token_type;
        self.parse_precedence(PrecUnary);

        match operation_type {
            TokenMinus => self.emit_byte(OpNegate),
            _ => todo!(),
        }
    }

    fn binary(&mut self) {
        let tk = self.current;
        let rule = self.get_rule(tk.token_type);

        self.parse_precedence(rule.precedence.next());

        match tk.token_type {
            TokenMinus => self.emit_byte(OpSubtract),
            TokenPlus => self.emit_byte(OpAdd),
            TokenSlash => self.emit_byte(OpDivide),
            TokenStar => self.emit_byte(OpMultiply),
            _ => self.error_at(tk, "not support"),
        }
    }

    /// 因为 precedence 是针对 operator 来定义的，所以如果能找到更大 precedence 的 rule，
    /// 就意味着它是一个操作符，而所有调用 parse_precedence 的地方都是在操作符的处理上，
    /// 一个操作符，要么是再接一个更高优先级的操作符，要么是一个非操作符。如果是一个操作符，那么一定是 prefix 的。
    /// 所以它会先调用 prefix，继续递归。如果不是一个操作符，那就是一个操作数。而所有的操作数都有 prefix，
    /// 就是构造它们自身。操作数或者操作符的 prefix 处理完以后，会继续看接下来更高优先级的操作符，
    /// 如果有，意味着它要继续处理这个操作数，所以它一定会是一个 infix（定义上决定的），所以就不需要判断空了。
    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let rule: &ParseRule<'_, '_> = self.get_rule(self.current.token_type);
        if let Some(prefix) = rule.prefix {
            prefix(self);

            while precedence <= self.get_rule(self.next.token_type).precedence {
                self.advance();
                let rule = self.get_rule(self.current.token_type);
                rule.infix.expect("should have infix")(self);
            }
        } else {
            println!("{:?}", rule);
            self.error_at(self.current, "Expect expression.");
            return;
        }
    }

    fn get_rule(&self, token_type: TokenType) -> &ParseRule<'code, 'tk> {
        let rule = self.rules[token_type as usize].as_ref().expect(
            format!(
                "rules should be init will all token type, but missing {:?}",
                token_type
            )
            .as_str(),
        );
        return rule;
    }
}

fn init_rules<'code, 'tk>() -> [Option<ParseRule<'code, 'tk>>; RULE_LENGTH] {
    let mut rules: [Option<ParseRule<'code, 'tk>>; RULE_LENGTH] = [const { None }; RULE_LENGTH];
    let temp_rules = [
        (
            TokenError,
            None as Option<ParseFun<'code, 'tk>>,
            None as Option<ParseFun<'code, 'tk>>,
            PrecNone,
        ),
        (TokenNumber, Some(Parser::number), None, PrecNone),
        (TokenLeftParen, Some(Parser::grouping), None, PrecNone),
        (
            TokenMinus,
            Some(Parser::unary),
            Some(Parser::binary),
            PrecTerm,
        ),
        (TokenPlus, None, Some(Parser::binary), PrecTerm),
        (TokenStar, None, Some(Parser::binary), PrecFactor),
        (TokenSlash, None, Some(Parser::binary), PrecFactor),
        (TokenEOF, None, None, PrecNone),
    ];

    for (tk_type, prefix, infix, precedence) in temp_rules {
        let rule = ParseRule {
            prefix,
            infix,
            precedence,
        };

        rules[tk_type as usize] = Some(rule);
    }

    return rules;
}

pub fn compile(source: &str) -> Option<Chunk> {
    return Parser::new(source).compile();
}

#[cfg(test)]
mod tests {
    use crate::{compiler::compile, scanner::TokenType};

    #[test]
    fn tdd() {}
}
