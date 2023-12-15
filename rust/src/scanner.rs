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
                "(" => self.token(TokenLeftParen),
                ")" => self.token(TokenRightParen),
                "{" => self.token(TokenLeftBrace),
                "}" => self.token(TokenRightBrace),
                "," => self.token(TokenComma),
                "." => self.token(TokenDot),
                "-" => self.token(TokenMinus),
                "+" => self.token(TokenPlus),
                ";" => self.token(TokenSemicolon),
                "/" => {
                    if self.match_next("/") {
                        self.advance_till("\n");
                        continue;
                    } else {
                        self.token(TokenSlash)
                    }
                }
                "*" => self.token(TokenStar),
                "!" => self.handle_one_more("=", TokenBangEqual, TokenBang),
                "=" => self.handle_one_more("=", TokenEqualEqual, TokenEqual),
                ">" => self.handle_one_more("=", TokenGreaterEqual, TokenGreater),
                "<" => self.handle_one_more("=", TokenLessEqual, TokenLess),
                " " | "\t" | "\r" => {
                    self.advance();
                    continue;
                }
                "\n" => {
                    self.line += 1;
                    self.advance();
                    continue;
                }
                "\"" => self.handle_string(),
                one if is_digital(one) => self.handle_number(),
                one if is_alpha(one) => self.handle_keywords_and_identifier(),
                _ => self.token(TokenError),
            };

            self.advance();
            return tk;
        }
    }

    fn current_char(&self) -> &str {
        let end = std::cmp::min(self.source.len(), self.current + 1);
        return &self.source[self.current..end];
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
        item: &str,
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

    fn match_next(&self, item: &str) -> bool {
        return self.check_index(self.current + 1, |next| next == item);
    }

    fn advance(&mut self) {
        if !self.at_the_end() {
            self.current += 1;
        }
    }

    fn advance_till(&mut self, terminator: &str) -> bool {
        let mut matched = false;
        while !self.at_the_end() && !matched {
            matched = self.match_next(terminator);

            if self.current_char() == "\n" {
                self.line += 1;
            }
            self.advance();
        }

        return matched;
    }

    fn handle_string(&mut self) -> Token<'tk> {
        if self.advance_till("\"") {
            self.advance();
            return self.token(TokenType::TokenString);
        } else {
            return self.error_token("Unterminated string.");
        }
    }

    fn handle_number(&mut self) -> Token<'tk> {
        while self.check_index(self.current + 1, is_digital) {
            self.advance();
        }

        if self.match_next(".") && self.check_index(self.current + 2, is_digital) {
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
            "a" => self.check_keyword(1, "nd", TokenAnd),
            "c" => self.check_keyword(1, "lass", TokenClass),
            "e" => self.check_keyword(1, "lse", TokenElse),
            "i" => self.check_keyword(1, "f", TokenIf),
            "n" => self.check_keyword(1, "il", TokenNil),
            "o" => self.check_keyword(1, "r", TokenOr),
            "p" => self.check_keyword(1, "rint", TokenPrint),
            "r" => self.check_keyword(1, "eturn", TokenReturn),
            "s" => self.check_keyword(1, "uper", TokenSuper),
            "v" => self.check_keyword(1, "ar", TokenVar),
            "w" => self.check_keyword(1, "hile", TokenWhile),
            "f" => match self.get_from_each_start(1, 1) {
                "a" => self.check_keyword(2, "lse", TokenFalse),
                "o" => self.check_keyword(2, "r", TokenFor),
                "u" => self.check_keyword(2, "n", TokenFun),
                _ => TokenType::TokenIdentifier,
            },
            "t" => match self.get_from_each_start(1, 1) {
                "h" => self.check_keyword(2, "is", TokenThis),
                "r" => self.check_keyword(2, "ue", TokenTrue),
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

    fn check_index(&self, idx: usize, check_fn: impl Fn(&str) -> bool) -> bool {
        let start = std::cmp::min(self.source.len(), idx);
        let end = std::cmp::min(self.source.len(), start + 1);
        return check_fn(&self.source[start..end]);
    }

    fn check_keyword(&self, start: usize, rest: &str, kw_type: TokenType) -> TokenType {
        let lexeme = self.current_lexeme();
        let length = rest.len();
        if lexeme.len() == start + length && &lexeme[start..] == rest {
            return kw_type;
        }

        return TokenType::TokenIdentifier;
    }

    fn get_from_each_start(&self, start: usize, delta: usize) -> &str {
        let lexeme = self.current_lexeme();
        if lexeme.len() < start + delta {
            return "";
        }
        return &lexeme[start..start + delta];
    }
}

fn is_alpha(one: &str) -> bool {
    one.len() == 1 && ((one >= "a" && one <= "z") || (one >= "A" && one <= "Z") || one == "_")
}

fn is_digital(one: &str) -> bool {
    one.len() == 1 && one >= "0" && one <= "9"
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
