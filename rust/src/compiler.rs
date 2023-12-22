use crate::{
    chunk::{Chunk, Value},
    op::OpCode::*,
    scanner::{
        Scanner, Token,
        TokenType::{self, *},
        TOKEN_PLACEHOLDER,
    },
    vm::Heap,
};

#[derive(PartialEq, PartialOrd)]
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
            PrecPrimary => panic!("unreachable"),
        }
    }
}

use Precedence::*;

type ParseFun<'code, 'tk> = fn(&mut Parser<'code, 'tk>);

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
    heap: &'code mut Heap,
}

impl<'code, 'tk> Parser<'code, 'tk> {
    pub fn new(source: &'code str, heap: &'code mut Heap) -> Self {
        let rules: [Option<ParseRule<'_, '_>>; 40] = init_rules();
        let scanner = Scanner::new(source);
        Self {
            chunk: Chunk::new(),
            scanner,
            had_error: false,
            panic_mode: false,
            current: TOKEN_PLACEHOLDER.clone(),
            next: TOKEN_PLACEHOLDER.clone(),
            rules: rules,
            heap: heap,
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

    fn emit_bytes<T1: Into<u8>, T2: Into<u8>>(&mut self, byte1: T1, byte2: T2) {
        self.emit_byte(byte1);
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
        let tk = self.next.clone();
        if tk.token_type == match_type {
            self.advance();
            return;
        }

        self.error_at(tk, msg);
    }

    fn advance(&mut self) {
        self.current = self.next.clone();

        loop {
            let tk: Token<'_> = self.scanner.scan_token();
            if !tk.is_error_tk() {
                self.next = tk;
                break;
            }

            let msg = tk.lexeme;
            self.error_at(tk, msg);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(PrecAssignment);
    }

    fn number(&mut self) {
        let tk = self.current.clone();
        let number = tk.lexeme.parse::<f64>().unwrap();
        self.emit_constant(tk, Value::NUMBER(number));
    }

    fn string(&mut self) {
        let tk = self.current.clone();
        let constant = self
            .heap
            .allocate_string(&tk.lexeme[1..tk.lexeme.len() - 1]);
        self.emit_constant(tk, constant);
    }

    fn literal(&mut self) {
        let tk = self.current.clone();
        match tk.token_type {
            TokenTrue => self.emit_byte(OpTrue),
            TokenFalse => self.emit_byte(OpFalse),
            TokenNil => self.emit_byte(OpNil),
            _ => panic!("not support {:?}", tk.token_type),
        }
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
            TokenBang => self.emit_byte(OpNot),
            _ => panic!("not support {:?}", operation_type),
        }
    }

    fn binary(&mut self) {
        let tk = self.current.clone();
        let rule = self.get_rule(tk.token_type);

        self.parse_precedence(rule.precedence.next());

        match tk.token_type {
            TokenMinus => self.emit_byte(OpSubtract),
            TokenPlus => self.emit_byte(OpAdd),
            TokenSlash => self.emit_byte(OpDivide),
            TokenStar => self.emit_byte(OpMultiply),
            TokenEqualEqual => self.emit_byte(OpEqual),
            TokenBangEqual => self.emit_bytes(OpEqual, OpNot),
            TokenGreater => self.emit_byte(OpGreater),
            TokenGreaterEqual => self.emit_bytes(OpLess, OpNot),
            TokenLess => self.emit_byte(OpLess),
            TokenLessEqual => self.emit_bytes(OpGreater, OpNot),
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
            self.error_at(self.current.clone(), "Expect expression.");
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

    fn emit_constant(&mut self, tk: Token, constant: Value) {
        let idx = self.chunk.add_constant(constant);
        if idx > u8::MAX.into() {
            self.error_at(tk, "Too many constants in one chunk.");
        }
        self.emit_bytes(OpConstant, idx as u8)
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
        (TokenLeftParen, Some(Parser::grouping), None, PrecNone),
        (TokenRightParen, None, None, PrecNone),
        (TokenLeftBrace, None, None, PrecNone),
        (TokenRightBrace, None, None, PrecNone),
        (TokenComma, None, None, PrecNone),
        (TokenDot, None, None, PrecNone),
        (
            TokenMinus,
            Some(Parser::unary),
            Some(Parser::binary),
            PrecTerm,
        ),
        (TokenPlus, None, Some(Parser::binary), PrecTerm),
        (TokenSemicolon, None, None, PrecNone),
        (TokenSlash, None, Some(Parser::binary), PrecFactor),
        (TokenStar, None, Some(Parser::binary), PrecFactor),
        (TokenBang, Some(Parser::unary), None, PrecUnary),
        (TokenBangEqual, None, Some(Parser::binary), PrecEquality),
        (TokenEqual, None, None, PrecNone),
        (TokenEqualEqual, None, Some(Parser::binary), PrecEquality),
        (TokenGreater, None, Some(Parser::binary), PrecComparison),
        (
            TokenGreaterEqual,
            None,
            Some(Parser::binary),
            PrecComparison,
        ),
        (TokenLess, None, Some(Parser::binary), PrecComparison),
        (TokenLessEqual, None, Some(Parser::binary), PrecComparison),
        (TokenIdentifier, None, None, PrecNone),
        (TokenString, Some(Parser::string), None, PrecNone),
        (TokenNumber, Some(Parser::number), None, PrecNone),
        (TokenAnd, None, None, PrecNone),
        (TokenClass, None, None, PrecNone),
        (TokenElse, None, None, PrecNone),
        (TokenFalse, Some(Parser::literal), None, PrecNone),
        (TokenFor, None, None, PrecNone),
        (TokenFun, None, None, PrecNone),
        (TokenIf, None, None, PrecNone),
        (TokenNil, Some(Parser::literal), None, PrecNone),
        (TokenOr, None, None, PrecNone),
        (TokenPrint, None, None, PrecNone),
        (TokenReturn, None, None, PrecNone),
        (TokenSuper, None, None, PrecNone),
        (TokenThis, None, None, PrecNone),
        (TokenTrue, Some(Parser::literal), None, PrecNone),
        (TokenVar, None, None, PrecNone),
        (TokenWhile, None, None, PrecNone),
        (TokenError, None, None, PrecNone),
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

pub fn compile(source: &str, heap: &mut Heap) -> Option<Chunk> {
    return Parser::new(source, heap).compile();
}

#[cfg(test)]
mod tests {

    #[test]
    fn tdd() {}
}