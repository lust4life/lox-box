use crate::{
    chunk::{Chunk, Value},
    op::OpCode,
};

enum InterpretResult {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
}

const STACK_MAX: usize = 256;

struct VM {
    chunk: Chunk,
    pc: usize,
    stack: [Value; STACK_MAX],
    stack_top_off_set: usize,
}

use InterpretResult::*;

fn format_value(value: Value) -> String {
    return format!("{value:.2}");
}

macro_rules! binary_op {
    ($self:ident, $op:tt) => {
        $self.binary_op(|l,r| l $op r)
    };
}

impl VM {
    fn new(chunk: Chunk) -> Self {
        return Self {
            chunk: chunk,
            pc: 0,
            stack: [Value::default(); STACK_MAX],
            stack_top_off_set: 0,
        };
    }

    #[cfg(test)]
    fn debug_trace_execution(&self) {
        print!("          ");
        for offset in 0..self.stack_top_off_set {
            let value_str = format_value(self.stack[offset]);
            print!("[ {value_str} ]")
        }
        println!("");
        self.chunk.disassemble_instruction(self.pc);
    }

    fn interpret(&mut self) -> InterpretResult {
        loop {
            self.debug_trace_execution();
            let instruction = self.read_byte();
            match instruction {
                OpCode::OpConstant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::OpNegate => {
                    let v = self.pop();
                    self.push(-v);
                }
                OpCode::OpAdd => binary_op!(self, +),
                OpCode::OpSubtract => binary_op!(self, -),
                OpCode::OpMultiply => binary_op!(self, *),
                OpCode::OpDivide => binary_op!(self, /),
                OpCode::OpReturn => {
                    println!("{}", format_value(self.pop()));
                    return InterpretOk;
                }
            }
        }
    }

    fn binary_op(&mut self, op: fn(Value, Value) -> Value) {
        let right = self.pop();
        let left = self.pop();
        let res = op(left, right);
        self.push(res);
    }

    fn read_byte<T>(&mut self) -> &T {
        let one = self.chunk.get_one(self.pc);
        self.pc += 1;
        return one;
    }

    fn read_constant(&mut self) -> Value {
        let constant = self.chunk.get_constant(self.pc);
        self.pc += 1;
        return constant;
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top_off_set] = value;
        self.stack_top_off_set += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top_off_set -= 1;
        return self.stack[self.stack_top_off_set];
    }
}

#[cfg(test)]
mod tests {
    use super::VM;
    use crate::{chunk::Chunk, op::OpCode};

    #[test]
    fn xxx() {
        let mut chunk = Chunk::new();
        chunk.write_chunk(OpCode::OpConstant as u8, 123);
        let idx = chunk.add_constant(3.0);
        chunk.write_chunk(idx as _, 123);

        chunk.write_chunk(OpCode::OpConstant as u8, 123);
        let idx = chunk.add_constant(2.0);
        chunk.write_chunk(idx as _, 123);

        chunk.write_chunk(OpCode::OpAdd as u8, 123);

        chunk.write_chunk(OpCode::OpConstant as u8, 123);

        let idx = chunk.add_constant(1.0);
        chunk.write_chunk(idx as _, 123);
        chunk.write_chunk(OpCode::OpSubtract as u8, 123);

        chunk.write_chunk(OpCode::OpReturn as u8, 123);

        let mut vm = VM::new(chunk);
        vm.interpret();
    }
}
