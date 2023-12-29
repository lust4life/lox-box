#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
    TokenLeftParen,
    TokenRightParen,
    TokenLeftBrace,
    TokenRightBrace,
    TokenComma,
    TokenDot,
    TokenMinus,
    TokenPlus,
    TokenSemicolon,
    TokenSlash,
    TokenStar,

    // One or two character tokens.
    TokenBang,
    TokenBangEqual,
    TokenEqual,
    TokenEqualEqual,
    TokenGreater,
    TokenGreaterEqual,
    TokenLess,
    TokenLessEqual,

    // Literals.
    TokenIdentifier,
    TokenString,
    TokenNumber,

    // Keywords.
    TokenAnd,
    TokenClass,
    TokenElse,
    TokenFalse,
    TokenFor,
    TokenFun,
    TokenIf,
    TokenNil,
    TokenOr,
    TokenPrint,
    TokenReturn,
    TokenSuper,
    TokenThis,
    TokenTrue,
    TokenVar,
    TokenWhile,

    TokenError,
    TokenEOF,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub lexeme: &'a str,
    pub token_type: TokenType,
    pub line: usize,
}

pub static TOKEN_PLACEHOLDER: Token = Token {
    lexeme: "should not occur",
    token_type: TokenType::TokenError,
    line: 0,
};

impl<'a> Token<'a> {
    pub fn is_error_tk(&self) -> bool {
        return self.token_type == TokenType::TokenError;
    }
}

pub struct Scanner<'code> {
    source: &'code str,
    current: usize,
    each_start: usize,
    line: usize,
}

impl<'code: 'tk, 'tk> Scanner<'code> {
    pub fn new(source: &'code str) -> Self {
        Self {
            source,
            current: 0,
            each_start: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token<'tk> {
        use TokenType::*;
        loop {
            self.each_start = self.current;

            if self.at_the_end() {
                return self.token(TokenEOF);
            }

            let current_char = self.current_char();
            let tk = match current_char {
                b"(" => self.token(TokenLeftParen),
                b")" => self.token(TokenRightParen),
                b"{" => self.token(TokenLeftBrace),
                b"}" => self.token(TokenRightBrace),
                b"," => self.token(TokenComma),
                b"." => self.token(TokenDot),
                b"-" => self.token(TokenMinus),
                b"+" => self.token(TokenPlus),
                b";" => self.token(TokenSemicolon),
                b"/" => {
                    if self.match_next(b"/") {
                        self.advance_till(b"\n");
                        continue;
                    } else {
                        self.token(TokenSlash)
                    }
                }
                b"*" => self.token(TokenStar),
                b"!" => self.handle_one_more(b"=", TokenBangEqual, TokenBang),
                b"=" => self.handle_one_more(b"=", TokenEqualEqual, TokenEqual),
                b">" => self.handle_one_more(b"=", TokenGreaterEqual, TokenGreater),
                b"<" => self.handle_one_more(b"=", TokenLessEqual, TokenLess),
                b" " | b"\t" | b"\r" => {
                    self.advance();
                    continue;
                }
                b"\n" => {
                    self.line += 1;
                    self.advance();
                    continue;
                }
                b"\"" => self.handle_string(),
                one if is_digital(one) => self.handle_number(),
                one if is_alpha(one) => self.handle_keywords_and_identifier(),
                _ => self.error_token("Unexpected character."),
            };

            self.advance();
            return tk;
        }
    }

    fn current_char(&self) -> &[u8] {
        let end = std::cmp::min(self.source.len(), self.current + 1);
        return &self.source.as_bytes()[self.current..end];
    }
    fn current_lexeme(&self) -> &'tk str {
        let end = std::cmp::min(self.source.len(), self.current + 1);
        return &self.source[self.each_start..end];
    }
    fn at_the_end(&self) -> bool {
        return self.source.len() == self.current;
    }

    fn token(&self, token_type: TokenType) -> Token<'tk> {
        let lexeme = self.current_lexeme();
        return Token {
            lexeme,
            token_type,
            line: self.line,
        };
    }

    fn handle_one_more(
        &mut self,
        item: &[u8],
        matched: TokenType,
        not_matched: TokenType,
    ) -> Token<'tk> {
        if self.match_next(item) {
            self.advance();
            return self.token(matched);
        } else {
            return self.token(not_matched);
        }
    }

    fn match_next(&self, item: &[u8]) -> bool {
        return self.check_index(self.current + 1, |next| next == item);
    }

    fn advance(&mut self) {
        if !self.at_the_end() {
            self.current += 1;
        }
    }

    fn advance_till(&mut self, terminator: &[u8]) -> bool {
        let mut matched = false;
        while !self.at_the_end() && !matched {
            matched = self.match_next(terminator);

            if self.current_char() == b"\n" {
                self.line += 1;
            }
            self.advance();
        }

        return matched;
    }

    fn handle_string(&mut self) -> Token<'tk> {
        if self.advance_till(b"\"") {
            return self.token(TokenType::TokenString);
        } else {
            return self.error_token("Unterminated string.");
        }
    }

    fn handle_number(&mut self) -> Token<'tk> {
        while self.check_index(self.current + 1, is_digital) {
            self.advance();
        }

        if self.match_next(b".") && self.check_index(self.current + 2, is_digital) {
            self.advance();

            while self.check_index(self.current + 1, is_digital) {
                self.advance();
            }
        }

        return self.token(TokenType::TokenNumber);
    }

    fn handle_keywords_and_identifier(&mut self) -> Token<'tk> {
        use TokenType::*;

        while self.check_index(self.current + 1, |next| is_alpha(next) || is_digital(next)) {
            self.advance();
        }

        let identifier_type = match self.get_from_each_start(0, 1) {
            b"a" => self.check_keyword(1, b"nd", TokenAnd),
            b"c" => self.check_keyword(1, b"lass", TokenClass),
            b"e" => self.check_keyword(1, b"lse", TokenElse),
            b"i" => self.check_keyword(1, b"f", TokenIf),
            b"n" => self.check_keyword(1, b"il", TokenNil),
            b"o" => self.check_keyword(1, b"r", TokenOr),
            b"p" => self.check_keyword(1, b"rint", TokenPrint),
            b"r" => self.check_keyword(1, b"eturn", TokenReturn),
            b"s" => self.check_keyword(1, b"uper", TokenSuper),
            b"v" => self.check_keyword(1, b"ar", TokenVar),
            b"w" => self.check_keyword(1, b"hile", TokenWhile),
            b"f" => match self.get_from_each_start(1, 1) {
                b"a" => self.check_keyword(2, b"lse", TokenFalse),
                b"o" => self.check_keyword(2, b"r", TokenFor),
                b"u" => self.check_keyword(2, b"n", TokenFun),
                _ => TokenType::TokenIdentifier,
            },
            b"t" => match self.get_from_each_start(1, 1) {
                b"h" => self.check_keyword(2, b"is", TokenThis),
                b"r" => self.check_keyword(2, b"ue", TokenTrue),
                _ => TokenType::TokenIdentifier,
            },
            _ => TokenType::TokenIdentifier,
        };

        return self.token(identifier_type);
    }

    fn error_token(&self, msg: &'static str) -> Token<'static> {
        let lexeme = msg;
        return Token {
            lexeme,
            token_type: TokenType::TokenError,
            line: self.line,
        };
    }

    fn check_index(&self, idx: usize, check_fn: impl Fn(&[u8]) -> bool) -> bool {
        let start = std::cmp::min(self.source.len(), idx);
        let end = std::cmp::min(self.source.len(), start + 1);
        return check_fn(&self.source.as_bytes()[start..end]);
    }

    fn check_keyword(&self, start: usize, rest: &[u8], kw_type: TokenType) -> TokenType {
        let lexeme = self.current_lexeme().as_bytes();
        let length = rest.len();
        if lexeme.len() == start + length && &lexeme[start..] == rest {
            return kw_type;
        }

        return TokenType::TokenIdentifier;
    }

    fn get_from_each_start(&self, start: usize, delta: usize) -> &[u8] {
        let lexeme = self.current_lexeme().as_bytes();
        if lexeme.len() < start + delta {
            return b"";
        }
        return &lexeme[start..start + delta];
    }
}

fn is_alpha(one: &[u8]) -> bool {
    one.len() == 1 && ((one >= b"a" && one <= b"z") || (one >= b"A" && one <= b"Z") || one == b"_")
}

fn is_digital(one: &[u8]) -> bool {
    one.len() == 1 && one >= b"0" && one <= b"9"
}

#[cfg(test)]
mod tests {
    use super::{Scanner, TokenType};

    #[test]
    fn tdd() {
        let mut scanner = Scanner::new(
            "+-*/ <= >= == = \t ! ( ) {} 123 456. 789.123 1.2.3
        // this is a comment
        if else and superb for fun false true this ttt
        ",
        );
        loop {
            let tk = scanner.scan_token();
            println!("{tk:?}");
            if tk.token_type == TokenType::TokenEOF {
                break;
            }
        }
    }
}
