use std::{ops::FromResidual, rc::Rc};

use crate::{
    chunk::{Chunk, Value},
    compiler::{self},
    object::{Obj, ObjString, ObjType},
    op::OpCode::{self, *},
    table::Table,
};

pub enum InterpretResult {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
}

use InterpretResult::*;

impl FromResidual<Option<std::convert::Infallible>> for InterpretResult {
    fn from_residual(_: Option<std::convert::Infallible>) -> Self {
        return InterpretCompileError;
    }
}

const STACK_MAX: usize = 256;

pub struct Heap {
    objects: Option<Rc<Obj>>,
    strings: Table,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            objects: None,
            strings: Table::new(),
        }
    }

    pub fn allocate_string(&mut self, content: &str) -> Value {
        let hash = ObjString::hash_string(content);
        if let Some(str_v) = self.strings.find_key(content, hash) {
            return Value::OBJ(str_v);
        }

        let obj_str = ObjString {
            chars: content.to_string(),
            hash,
        };
        let root = std::mem::take(&mut self.objects);
        let rc_obj = Rc::new(Obj::new_string(obj_str, root));
        self.strings.set(Rc::clone(&rc_obj), Value::NIL);

        self.objects = Some(Rc::clone(&rc_obj));
        let str_v = Value::OBJ(rc_obj);
        return str_v;
    }
}

pub struct VM {
    chunk: Chunk,
    pc: usize,
    stack: [Value; STACK_MAX],
    stack_top_off_set: usize,
    heap: Heap,
}

pub fn interpret(source: &str) -> InterpretResult {
    let mut heap = Heap::new();
    let chunk = compiler::compile(source, &mut heap)?;
    let mut vm = VM::new(chunk, heap);
    return vm.run();
}

impl VM {
    pub fn new(chunk: Chunk, heap: Heap) -> Self {
        return Self {
            chunk: chunk,
            pc: 0,
            stack: [const { Value::NIL }; STACK_MAX],
            stack_top_off_set: 0,
            heap,
        };
    }

    fn debug_trace_execution(&self) {
        print!("          ");
        for offset in 0..self.stack_top_off_set {
            let value = &self.stack[offset];
            print!("[ {} ]", value)
        }
        println!("");
        self.chunk.disassemble_instruction(self.pc);
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if cfg!(test) {
                self.debug_trace_execution();
            }
            let instruction = self.read_byte::<OpCode>();
            match instruction {
                OpConstant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpNegate => {
                    let v = -self.pop();
                    match v {
                        Ok(v) => self.push(v),
                        Err(msg) => return self.runtime_error(&msg),
                    }
                }
                OpAdd | OpSubtract | OpMultiply | OpDivide | OpLess | OpGreater => {
                    match self.binary_op_for_numbers(instruction) {
                        Ok(value) => {
                            self.push(value);
                        }
                        Err(msg) => return self.runtime_error(&msg),
                    }
                }
                OpEqual => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Value::BOOL(lhs == rhs));
                }
                OpReturn => {
                    println!("{}", self.pop());
                    return InterpretResult::InterpretOk;
                }
                OpNot => {
                    let v = !self.pop().cast_truthy();
                    self.push(Value::BOOL(v));
                }
                OpTrue => {
                    self.push(Value::BOOL(true));
                }
                OpFalse => {
                    self.push(Value::BOOL(false));
                }
                OpNil => {
                    self.push(Value::NIL);
                }
            }
        }
    }

    fn binary_op_for_numbers(&mut self, instruction: OpCode) -> Result<Value, String> {
        use Value::*;
        let rhs = self.pop();
        let lhs = self.pop();

        match (lhs, rhs) {
            (NUMBER(lhs), NUMBER(rhs)) => {
                let res = match instruction {
                    OpAdd => NUMBER(lhs + rhs),
                    OpSubtract => NUMBER(lhs - rhs),
                    OpMultiply => NUMBER(lhs * rhs),
                    OpDivide => NUMBER(lhs / rhs),
                    OpGreater => BOOL(lhs > rhs),
                    OpLess => BOOL(lhs < rhs),
                    _ => panic!("not support {:?}", instruction),
                };

                return Ok(res);
            }
            (OBJ(lhs), OBJ(rhs)) => match (&lhs.ty, &rhs.ty) {
                (ObjType::ObjString(lhs), ObjType::ObjString(rhs)) => {
                    let concated = lhs.chars.clone() + &rhs.chars;
                    let res = self.heap.allocate_string(concated.as_str());
                    return Ok(res);
                }
            },
            _ => return Err("Operands must be two numbers or two strings.".to_owned()),
        }
    }

    fn read_byte<T: Copy>(&mut self) -> T {
        let one = self.chunk.get_one::<T>(self.pc);
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
        return self.stack[self.stack_top_off_set].clone();
    }

    fn runtime_error(&self, msg: &str) -> InterpretResult {
        eprintln!("{}", msg);
        let current_line = self.chunk.get_line(self.pc - 1);
        eprintln!("[line {current_line}] in script");
        return InterpretRuntimeError;
    }
}

#[cfg(test)]
mod tests {

    use super::interpret;

    #[test]
    fn xxx() {
        interpret(r#" "123" + "-" +"456" == "123-456" "#);
    }
}
