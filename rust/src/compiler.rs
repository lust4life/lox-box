use std::{borrow::BorrowMut, rc::Rc};

use crate::{
    chunk::{Chunk, Value},
    object::{Obj, ObjFunction, ObjString},
    op::OpCode::{self, *},
    scanner::{
        Scanner, Token,
        TokenType::{self, *},
        TOKEN_PLACEHOLDER,
    },
    vm::Heap,
};

#[derive(PartialEq, PartialOrd, Clone, Copy)]
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

const LOCAL_STACK_MAX_COUNT: usize = u8::MAX as usize + 1;
struct Compiler<'tk> {
    locals: [Option<Local<'tk>>; LOCAL_STACK_MAX_COUNT],
    local_count: usize,
    current_scope: i8,

    function: Option<ObjFunction>,
    enclosing: Option<Box<Compiler<'tk>>>,
}
impl<'tk> Compiler<'tk> {
    fn _new(enclosing: Option<Compiler<'tk>>, function: Option<ObjFunction>) -> Self {
        Self {
            locals: [const { None }; LOCAL_STACK_MAX_COUNT],
            current_scope: 0,
            local_count: 0,
            function: function,
            enclosing: enclosing.map(Box::new),
        }
    }

    fn new() -> Self {
        Self::_new(None, None)
    }

    fn new_with_enclosing(enclosing: Compiler<'tk>, function: ObjFunction) -> Self {
        Self::_new(Some(enclosing), Some(function))
    }

    fn within_scope(&self) -> bool {
        self.current_scope > 0
    }

    fn mark_local_initialized(&mut self) {
        self.locals[self.local_count - 1].as_mut().unwrap().scope = self.current_scope;
    }

    fn resolve_local(&mut self, tk: &Token) -> Result<Option<u8>, String> {
        for idx in (0..self.local_count).rev() {
            let local = self.locals[idx].as_ref().unwrap();
            if local.name == tk.lexeme {
                if local.scope == -1 {
                    return Err("Can't read local variable in its own initializer.".to_string());
                }
                return Ok(Some(idx as _));
            }
        }

        return Ok(None);
    }

    fn declare_local(&mut self, tk: &Token<'tk>) -> Result<(), String> {
        if self.local_count == LOCAL_STACK_MAX_COUNT {
            return Err("Too many local variables in function.".to_string());
        }

        for local in self.from_end(&self.locals) {
            if local.scope < self.current_scope {
                break;
            }

            let existed = local.scope == self.current_scope && local.name == tk.lexeme;
            if existed {
                return Err("Already a variable with this name in this scope.".to_string());
            }
        }

        self.add_local(tk.lexeme);

        return Ok(());
    }

    fn add_local(&mut self, name: &'tk str) {
        self.locals[self.local_count] = Some(Local {
            name: name,
            scope: -1,
        });
        self.local_count += 1;
    }

    fn from_end<'locals>(
        &self,
        locals: &'locals [Option<Local<'tk>>],
    ) -> impl Iterator<Item = &'locals Local<'tk>> {
        locals[0..self.local_count]
            .iter()
            .rev()
            .map(|x| x.as_ref().unwrap())
    }

    fn end_scope(&mut self) -> usize {
        let before = self.local_count;
        for local in self.from_end(&self.locals) {
            if local.scope >= self.current_scope {
                self.local_count -= 1;
            } else {
                break;
            }
        }

        self.current_scope -= 1;
        return before - self.local_count;
    }

    fn begin_parse_function(&mut self, name: Rc<ObjString>) {
        let current = std::mem::replace(self, Compiler::new());
        *self = Compiler::new_with_enclosing(current, ObjFunction::new(name));
    }

    fn end_parse_function(&mut self, func_arity: usize) -> ObjFunction {
        let current = std::mem::replace(self, Compiler::new());
        let mut parsed_function = current
            .function
            .expect("should call begin_parse_function to set function first");
        parsed_function.arity = func_arity;
        *self = *current.enclosing.unwrap();
        return parsed_function;
    }
}

struct Local<'tk> {
    name: &'tk str,
    scope: i8,
}

type ParseFun<'code, 'tk> = fn(&mut Parser<'code, 'tk>);

struct ParseRule<'code, 'tk> {
    prefix: Option<ParseFun<'code, 'tk>>,
    infix: Option<ParseFun<'code, 'tk>>,
    precedence: Precedence,
}

const RULE_LENGTH: usize = TokenEOF as usize + 1usize;
struct Parser<'code: 'tk, 'tk> {
    scanner: Scanner<'code>,

    parse_rules: [Option<ParseRule<'code, 'tk>>; RULE_LENGTH],
    current_precedence: Precedence,

    had_error: bool,
    panic_mode: bool,
    current: Token<'tk>,
    next: Token<'tk>,

    heap: &'code mut Heap,
    chunk: Chunk,            // the whole programe
    compiler: Compiler<'tk>, // contains function on the fly when compile
}

impl<'code, 'tk> Parser<'code, 'tk> {
    pub fn new(source: &'code str, heap: &'code mut Heap) -> Self {
        let rules = init_rules();
        let scanner = Scanner::new(source);
        Self {
            chunk: Chunk::new(),
            scanner,
            had_error: false,
            panic_mode: false,
            current: TOKEN_PLACEHOLDER.clone(),
            next: TOKEN_PLACEHOLDER.clone(),
            parse_rules: rules,
            heap: heap,
            current_precedence: PrecNone,
            compiler: Compiler::new(),
        }
    }

    fn compile(mut self) -> Option<Chunk> {
        self.advance();

        loop {
            let next_tk = self.next.clone();
            if next_tk.token_type == TokenEOF {
                break;
            }

            self.declaration();
        }

        self.consume(TokenEOF, "Expect end of expression.");

        if self.had_error {
            return None;
        }

        self.emit_byte(OpReturn);
        return Some(self.chunk);
    }

    fn active_chunk(&mut self) -> &mut Chunk {
        return self
            .compiler
            .function
            .as_mut()
            .map_or(self.chunk.borrow_mut(), |x| x.chunk.borrow_mut());
    }

    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        let line = self.current.line;
        self.active_chunk().write_chunk(byte.into(), line);
    }

    fn emit_bytes<T1: Into<u8>, T2: Into<u8>>(&mut self, byte1: T1, byte2: T2) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn error_at(&mut self, tk: &Token, msg: &str) {
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

        self.error_at(&tk, msg);
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
            self.error_at(&tk, msg);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(PrecAssignment);
    }

    fn number(&mut self) {
        let tk = self.current.clone();
        let number = tk.lexeme.parse::<f64>().unwrap();
        self.emit_constant(&tk, Value::NUMBER(number));
    }

    fn and(&mut self) {
        let and_jump = self.emit_jump(OpJumpIfFalse);

        self.emit_byte(OpPop);
        self.parse_precedence(PrecAnd);

        self.patch_jump(and_jump)
    }

    fn or(&mut self) {
        let skip_end_jump = self.emit_jump(OpJumpIfFalse);
        let end_jump = self.emit_jump(OpJump);
        self.patch_jump(skip_end_jump);

        self.emit_byte(OpPop);
        self.parse_precedence(PrecOr);

        self.patch_jump(end_jump);
    }

    fn string(&mut self) {
        let tk = self.current.clone();
        let constant = self
            .heap
            .allocate_string(&tk.lexeme[1..tk.lexeme.len() - 1]);
        self.emit_constant(&tk, constant);
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
            _ => self.error_at(&tk, "not support"),
        }
    }

    /// 因为 precedence 是针对 operator 来定义的，所以如果能找到更大 precedence 的 rule，
    /// 就意味着它是一个操作符，而所有调用 parse_precedence 的地方都是在操作符的处理上，
    /// 一个操作符，要么是再接一个更高优先级的操作符，要么是一个非操作符。如果是一个操作符，那么一定是 prefix 的。
    /// 所以它会先调用 prefix，继续递归。如果不是一个操作符，那就是一个操作数。而所有的操作数都有 prefix，
    /// 就是构造它们自身。操作数或者操作符的 prefix 处理完以后，会继续看接下来更高优先级的操作符，
    /// 如果有，意味着它要继续处理这个操作数，所以它一定会是一个 infix（定义上决定的），所以就不需要判断空了。
    fn parse_precedence(&mut self, precedence: Precedence) {
        let previous = self.current_precedence;
        self.current_precedence = precedence;
        self.advance();

        let rule: &ParseRule<'_, '_> = self.get_rule(self.current.token_type);
        if let Some(prefix) = rule.prefix {
            prefix(self);

            while precedence <= self.get_rule(self.next.token_type).precedence {
                self.advance();
                let rule = self.get_rule(self.current.token_type);
                rule.infix.expect("should have infix")(self);
            }

            if self.can_assign() && self.match_and_advance(TokenEqual) {
                self.error_at(&self.current.clone(), "Invalid assignment target.");
            }
        } else {
            self.error_at(&self.current.clone(), "Expect expression.");
        }

        self.current_precedence = previous;
    }

    fn get_rule(&self, token_type: TokenType) -> &ParseRule<'code, 'tk> {
        let rule = self.parse_rules[token_type as usize].as_ref().expect(
            format!(
                "rules should be init will all token type, but missing {:?}",
                token_type
            )
            .as_str(),
        );
        return rule;
    }

    fn emit_constant(&mut self, tk: &Token, constant: Value) {
        let idx = self.make_constant(constant, tk);
        self.emit_bytes(OpConstant, idx)
    }

    fn make_constant(&mut self, constant: Value, tk: &Token<'_>) -> u8 {
        let idx = self.active_chunk().add_constant(constant);
        if idx > u8::MAX.into() {
            self.error_at(tk, "Too many constants in one chunk.");
            return 0;
        }
        return idx as _;
    }

    fn declaration(&mut self) {
        if !self.var_declar() && !self.fun_declar() {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if !self.print_stmt()
            && !self.block_stmt()
            && !self.if_stmt()
            && !self.while_stmt()
            && !self.for_stmt()
        {
            self.expression_stmt();
        }
    }

    fn print_stmt(&mut self) -> bool {
        if self.match_and_advance(TokenPrint) {
            self.expression();
            self.consume(TokenSemicolon, "Expect ';' after value.");
            self.emit_byte(OpPrint);
            return true;
        }
        return false;
    }

    fn if_stmt(&mut self) -> bool {
        if self.match_and_advance(TokenIf) {
            self.consume(TokenLeftParen, "Expect '(' after 'if'.");
            self.expression();
            self.consume(TokenRightParen, "Expect ')' after condition.");

            let then_jump = self.emit_jump(OpJumpIfFalse);
            // cause we don't pop when check condition for OpJumpIfFalse in vm
            self.emit_byte(OpPop);

            self.statement();

            let else_jump = self.emit_jump(OpJump);

            self.patch_jump(then_jump);
            self.emit_byte(OpPop);

            if self.match_and_advance(TokenElse) {
                self.statement();
            }
            self.patch_jump(else_jump);

            return true;
        }
        return false;
    }

    fn block_stmt(&mut self) -> bool {
        if self.match_and_advance(TokenLeftBrace) {
            self.begin_scope();

            loop {
                let token_type = self.next.token_type;
                if !(token_type != TokenEOF && token_type != TokenRightBrace) {
                    break;
                }
                self.declaration();
            }

            self.consume(TokenRightBrace, "Expect '}' after block.");

            self.end_scope();
            return true;
        }
        return false;
    }

    fn while_stmt(&mut self) -> bool {
        if self.match_and_advance(TokenWhile) {
            let loop_start = self.active_chunk().code_count();

            self.consume(TokenLeftParen, "Expect '(' after while.");
            self.expression();
            self.consume(TokenRightParen, "Expect ')' after condition.");

            let exit_jump = self.emit_jump(OpJumpIfFalse);

            self.emit_byte(OpPop);
            self.statement();

            self.emit_loop(loop_start);

            self.patch_jump(exit_jump);

            return true;
        }
        return false;
    }

    fn for_stmt(&mut self) -> bool {
        if self.match_and_advance(TokenFor) {
            self.begin_scope();

            self.consume(TokenLeftParen, "Expect '(' after for.");

            if !self.var_declar() && !self.match_and_advance(TokenSemicolon) {
                self.expression_stmt();
            }

            let mut loop_start = self.active_chunk().code_count();

            let mut exit_jump: Option<usize> = None;
            if !self.match_and_advance(TokenSemicolon) {
                self.expression();
                self.consume(TokenSemicolon, "Expect ';' after loop condition.");
                exit_jump = Some(self.emit_jump(OpJumpIfFalse));
                self.emit_byte(OpPop);
            }

            if !self.match_and_advance(TokenRightParen) {
                let body_jump = self.emit_jump(OpJump);

                let increment_start = self.active_chunk().code_count();

                self.expression();
                self.emit_byte(OpPop);
                self.consume(TokenRightParen, "Expect ')' after clauses.");

                self.emit_loop(loop_start);
                loop_start = increment_start;

                self.patch_jump(body_jump);
            }

            self.statement();

            self.emit_loop(loop_start);

            if let Some(exit_jump) = exit_jump {
                self.patch_jump(exit_jump);
                self.emit_byte(OpPop);
            }

            self.end_scope();
            return true;
        }
        return false;
    }

    fn match_and_advance(&mut self, tk_type: TokenType) -> bool {
        let matched = tk_type == self.next.token_type;
        if matched {
            self.advance();
        }
        return matched;
    }

    fn expression_stmt(&mut self) {
        self.expression();
        self.consume(TokenSemicolon, "Expect ';' after expression.");
        self.emit_byte(OpPop);
    }

    fn var_declar(&mut self) -> bool {
        let matched = self.match_and_advance(TokenVar);
        if matched {
            let idx = self.parse_variable("Expect variable name.");
            if self.match_and_advance(TokenEqual) {
                self.expression();
            } else {
                self.emit_byte(OpNil);
            }

            self.consume(TokenSemicolon, "Expect ';' after value.");

            self.define_variable(idx);
        }

        return matched;
    }

    fn parse_variable(&mut self, msg: &str) -> Option<u8> {
        self.consume(TokenIdentifier, msg);
        let tk = &self.current.clone();

        if self.compiler.within_scope() {
            self.compiler
                .declare_local(tk)
                .unwrap_or_else(|ref msg| self.error_at(tk, msg));
            return None;
        } else {
            let idx = self.identifier_constant(tk);
            return Some(idx);
        }
    }

    fn identifier_constant(&mut self, tk: &Token) -> u8 {
        let constant = self.heap.allocate_string(tk.lexeme);
        let idx = self.make_constant(constant, tk);
        idx
    }

    fn variable(&mut self) {
        let (idx, get_op, set_op);
        let tk = &self.current.clone();

        match self.compiler.resolve_local(tk) {
            Ok(Some(local_idx)) => {
                idx = local_idx;
                get_op = OpGetLocal;
                set_op = OpSetLocal;
            }
            Ok(None) => {
                idx = self.identifier_constant(tk);
                get_op = OpGetGlobal;
                set_op = OpSetGlobal;
            }
            Err(ref msg) => {
                self.error_at(tk, msg);
                return;
            }
        }

        if self.can_assign() && self.match_and_advance(TokenEqual) {
            self.expression();
            self.emit_bytes(set_op, idx);
        } else {
            self.emit_bytes(get_op, idx);
        }
    }

    fn synchronize(&mut self) {
        loop {
            let token_type = self.current.token_type;
            if token_type != TokenEOF && token_type != TokenSemicolon {
                self.advance();
            } else {
                break;
            }
        }

        self.panic_mode = false;
    }

    fn can_assign(&self) -> bool {
        self.current_precedence <= PrecAssignment
    }

    fn begin_scope(&mut self) {
        self.compiler.current_scope += 1;
    }

    fn end_scope(&mut self) {
        let pop_count = self.compiler.end_scope();
        for _ in 0..pop_count {
            self.emit_byte(OpPop);
        }
    }

    fn define_variable(&mut self, global_idx: Option<u8>) {
        if let Some(global_idx) = global_idx {
            self.emit_bytes(OpDefineGlobal, global_idx);
        } else {
            self.compiler.mark_local_initialized();
        }
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        return self.active_chunk().code_count();
    }

    fn patch_jump(&mut self, start_offset: usize) {
        let end_offset = self.active_chunk().code_count();
        let delta = end_offset - start_offset;
        if delta > (u16::MAX as usize) {
            self.error_at(&self.current.clone(), "Too much code to jump over.");
            return;
        }

        let byte1 = (delta >> 8) as u8;
        let byte2 = delta as u8;

        self.active_chunk().set_one(start_offset - 2, byte1);
        self.active_chunk().set_one(start_offset - 1, byte2);
    }

    fn emit_loop(&mut self, start_offset: usize) {
        let end_offset = self.active_chunk().code_count();
        let delta = end_offset - start_offset + 3;
        if delta > (u16::MAX as usize) {
            self.error_at(&self.current.clone(), "Loop body too large.");
            return;
        }

        let byte1 = (delta >> 8) as u8;
        let byte2 = delta as u8;

        self.emit_byte(OpLoop);
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn fun_declar(&mut self) -> bool {
        let matched = self.match_and_advance(TokenFun);
        if matched {
            let idx = self.parse_variable("Expect function name.");
            let func_name_tk = self.current.clone();
            let func_name = self
                .heap
                .allocate_string(func_name_tk.lexeme)
                .cast_obj_string();
            self.compiler.begin_parse_function(func_name);

            let mut func_arity = 0;

            self.consume(TokenLeftParen, "Expect '(' after function.");

            if self.next.token_type != TokenRightParen {
                loop {
                    let idx = self.parse_variable("Expect parameter name.");
                    self.define_variable(idx);

                    func_arity += 1;
                    if func_arity > 255 {
                        self.error_at(
                            &self.current.clone(),
                            "Can't have more than 255 parameters.",
                        )
                    }

                    if !self.match_and_advance(TokenComma) {
                        break;
                    }
                }
            }

            self.consume(TokenRightParen, "Expect ')' after function.");

            self.block_stmt();

            let func = self.compiler.end_parse_function(func_arity);
            let func = self.heap.allocate_function(func);
            self.emit_constant(&func_name_tk, func);

            self.define_variable(idx);
        }

        return matched;
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
        (TokenIdentifier, Some(Parser::variable), None, PrecNone),
        (TokenString, Some(Parser::string), None, PrecNone),
        (TokenNumber, Some(Parser::number), None, PrecNone),
        (TokenAnd, None, Some(Parser::and), PrecAnd),
        (TokenClass, None, None, PrecNone),
        (TokenElse, None, None, PrecNone),
        (TokenFalse, Some(Parser::literal), None, PrecNone),
        (TokenFor, None, None, PrecNone),
        (TokenFun, None, None, PrecNone),
        (TokenIf, None, None, PrecNone),
        (TokenNil, Some(Parser::literal), None, PrecNone),
        (TokenOr, None, Some(Parser::or), PrecOr),
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
    fn tdd() {
        #[derive(Debug)]
        struct A {
            enclosing: Option<Box<A>>,
        }

        impl A {
            fn enclose(&mut self) {
                let prev_compiler = std::mem::replace(self, A { enclosing: None });
                *self = A {
                    enclosing: Some(Box::new(prev_compiler)),
                };
            }
        }
        let mut a = A { enclosing: None };
        dbg!(&a);
        a.enclose();
        dbg!(&a);
        a.enclose();
        dbg!(&a);
    }
}
