use std::{cell::RefCell, ops::FromResidual, process::ExitCode, rc::Rc};

use crate::{
    chunk::{Chunk, Value, Vec},
    compiler::{self},
    object::{
        Obj, ObjClosure, ObjFunction, ObjNative, ObjString, ObjType, ObjUpvalue, OpenUpvaule,
    },
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

impl FromResidual<std::io::Result<std::convert::Infallible>> for InterpretResult {
    fn from_residual(residual: std::io::Result<std::convert::Infallible>) -> Self {
        eprintln!("{}", residual.unwrap_err());
        std::process::exit(74);
    }
}

impl std::process::Termination for InterpretResult {
    fn report(self) -> std::process::ExitCode {
        match self {
            InterpretOk => ExitCode::SUCCESS,
            InterpretCompileError => ExitCode::from(65),
            InterpretRuntimeError => ExitCode::from(70),
        }
    }
}

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

    fn allocate(&mut self, ty: ObjType) -> Value {
        let root = std::mem::take(&mut self.objects);
        let rc_obj = Rc::new(Obj { ty: ty, next: root });
        self.objects = Some(Rc::clone(&rc_obj));
        let str_v = Value::OBJ(rc_obj);
        str_v
    }

    pub fn allocate_string(&mut self, content: &str) -> Value {
        let obj_str;

        let hash = ObjString::hash_string(content);
        if let Some(str_v) = self.strings.find_key(content, hash) {
            obj_str = str_v;
        } else {
            obj_str = Rc::new(ObjString {
                chars: content.to_string(),
                hash,
            });
            self.strings.set(Rc::clone(&obj_str), Value::NIL, false);
        }

        let v = self.allocate(ObjType::ObjString(obj_str));
        return v;
    }

    pub fn allocate_function(&mut self, func: ObjFunction) -> Value {
        let v = self.allocate(ObjType::ObjFunction(Rc::new(func)));
        return v;
    }

    fn allocate_closure(&mut self, func: Rc<ObjFunction>, upvalues: Vec<ObjUpvalue>) -> Value {
        return self.allocate(ObjType::ObjClosure(Rc::new(ObjClosure {
            function: func,
            upvalues,
        })));
    }
}

enum FrameType {
    Function(Rc<ObjClosure>),
    Script(Chunk),
}

impl FrameType {
    pub fn cast_closure(&self) -> Rc<ObjClosure> {
        match self {
            FrameType::Function(closure) => closure.clone(),
            FrameType::Script(_) => todo!(),
        }
    }
}

struct CallFrame {
    ty: FrameType,
    pc: usize,

    stack_offset: usize,
    open_upvalues: Option<Rc<RefCell<OpenUpvaule>>>,
}

impl CallFrame {
    fn new(ty: FrameType, stack_offset: usize) -> Self {
        Self {
            ty,
            pc: 0,
            stack_offset,
            open_upvalues: None,
        }
    }

    fn disassemble_instruction(&self) {
        self.chunk().disassemble_instruction(self.pc);
    }

    fn chunk(&self) -> &Chunk {
        match &self.ty {
            FrameType::Function(closure) => &closure.function.chunk,
            FrameType::Script(chunk) => chunk,
        }
    }

    fn get_one<T: Copy>(&mut self) -> T {
        let one = self.chunk().get_one(self.pc);
        self.pc += 1;
        return one;
    }

    fn get_constant(&mut self) -> Value {
        let constant = self.chunk().get_constant(self.pc);
        self.pc += 1;
        return constant;
    }

    fn jump(&mut self, delta: usize, backward: bool) {
        if backward {
            self.pc -= delta;
        } else {
            self.pc += delta;
        }
    }

    fn is_function(&self) -> bool {
        match self.ty {
            FrameType::Function(_) => true,
            FrameType::Script(_) => false,
        }
    }

    fn get_line(&self) -> usize {
        return self.chunk().get_line(self.pc - 1);
    }

    fn capture_upvalue(&mut self, idx: usize, value: Value) -> ObjUpvalue {
        // capture it on the callframe's open_upvalues

        let mut previous: Option<Rc<RefCell<OpenUpvaule>>> = None;
        let mut looped_open_upvalue = self.open_upvalues.clone();
        while let Some(inner) = looped_open_upvalue.clone() {
            let inner = inner.borrow();
            if inner.idx == idx {
                return inner.upvalue.clone();
            }

            if inner.idx < idx {
                break;
            }

            previous = looped_open_upvalue;
            looped_open_upvalue = inner.next.clone();
        }

        let open_upvalue = Rc::new(RefCell::new(OpenUpvaule::new(
            idx,
            value,
            looped_open_upvalue,
        )));
        let created_upvalue = open_upvalue.borrow().upvalue.clone();
        match previous {
            Some(previous) => {
                previous.borrow_mut().next = Some(open_upvalue);
            }
            None => {
                self.open_upvalues = Some(open_upvalue);
            }
        }

        return created_upvalue;
    }

    fn get_upvalue(&mut self, idx: usize) -> ObjUpvalue {
        return self.ty.cast_closure().upvalues[idx].clone();
    }

    fn set_upvalue(&self, idx: usize, value: Value) {
        *self.ty.cast_closure().upvalues[idx].borrow_mut() = value;
    }
}

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = u8::MAX as usize * FRAMES_MAX;

pub struct VM {
    stack: [Value; STACK_MAX],
    stack_count: usize,

    frames: [Option<CallFrame>; FRAMES_MAX],
    frame_count: usize,

    heap: Heap,
    globals: Table,
}

pub fn interpret(source: &str) -> InterpretResult {
    let mut heap = Heap::new();
    let chunk = compiler::compile(source, &mut heap)?;
    let mut vm = VM::new(chunk, heap);
    return vm.run();
}

fn clock_native(_args: &[Value]) -> Value {
    let unix_timestamp = std::time::UNIX_EPOCH.elapsed().unwrap().as_secs_f64();
    return Value::NUMBER(unix_timestamp);
}

impl VM {
    pub fn new(chunk: Chunk, heap: Heap) -> Self {
        let mut this = Self {
            stack: [const { Value::NIL }; STACK_MAX],
            stack_count: 0,
            heap,
            globals: Table::new(),
            frames: [const { None }; FRAMES_MAX],
            frame_count: 0,
        };

        this.define_native("clock", clock_native);

        let first_frame = CallFrame::new(FrameType::Script(chunk), this.stack_count);
        this.frames[0] = Some(first_frame);
        this.frame_count = 1;
        return this;
    }

    fn define_native(&mut self, name: &str, native_func: ObjNative) {
        let key = self.heap.allocate_string(name).cast_obj_string();
        let native_func = self.heap.allocate(ObjType::ObjNative(native_func));
        self.globals.set(key, native_func, false);
    }

    fn add_frame(&mut self, closure: Rc<ObjClosure>) {
        let frame_stack_offset = self.stack_count - closure.function.arity;
        let frame = CallFrame::new(FrameType::Function(closure), frame_stack_offset);
        self.frames[self.frame_count] = Some(frame);
        self.frame_count += 1;
    }

    fn current_frame(&mut self) -> &mut CallFrame {
        return self.frames[self.frame_count - 1].as_mut().unwrap();
    }

    fn debug_trace_execution(&mut self) {
        print!("          ");
        for offset in 0..self.stack_count {
            let value = &self.stack[offset];
            print!("[ {} ]", value)
        }
        println!();
        self.current_frame().disassemble_instruction();
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if cfg!(test) {
                self.debug_trace_execution();
            }
            let instruction = OpCode::from(self.read_byte());
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
                    let v = self.pop();

                    if self.current_frame().is_function() {
                        self.stack_count = self.current_frame().stack_offset;

                        self.close_upvalues();

                        self.pop(); // pop the fn itself

                        self.frame_count -= 1; // return to the upper frame

                        self.push(v);
                        continue;
                    }
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
                OpPrint => {
                    println!("{}", self.pop());
                }
                OpPop => {
                    self.pop();
                }
                OpDefineGlobal => {
                    let key = self.read_string();
                    // consider gc pause case here, peek first then pop after global set
                    let value = self.peek(0);
                    self.globals.set(key, value, false);
                    self.pop();
                }
                OpGetGlobal => {
                    let key = self.read_string();
                    match self.globals.get(&key) {
                        Some(value) => {
                            self.push(value.clone());
                        }
                        None => {
                            return self.runtime_error(
                                format!("Undefined variable '{}'.", key.chars).as_str(),
                            );
                        }
                    }
                }
                OpSetGlobal => {
                    let key = self.read_string();
                    let value = self.peek(0);
                    let exist = self.globals.set(key.clone(), value, true);
                    if !exist {
                        return self.runtime_error(
                            format!("Undefined variable '{}'.", key.chars).as_str(),
                        );
                    }
                }
                OpGetLocal => {
                    let slot = self.read_byte();
                    let slot = self.current_frame().stack_offset + slot as usize;
                    let value = self.stack[slot].clone();
                    self.push(value);
                }
                OpSetLocal => {
                    let slot = self.read_byte();
                    let slot = self.current_frame().stack_offset + slot as usize;
                    self.stack[slot] = self.peek(0);
                }
                OpJumpIfFalse => {
                    let condition = self.peek(0);
                    let delta = self.read_short();
                    if !condition.cast_truthy() {
                        self.current_frame().jump(delta, false);
                    }
                }
                OpJump => {
                    let delta = self.read_short();
                    self.current_frame().jump(delta, false);
                }
                OpLoop => {
                    let delta = self.read_short();
                    self.current_frame().jump(delta, true);
                }
                OpCall => {
                    let arg_count = self.read_byte();
                    if let Some(closure) = self.peek(arg_count).as_obj_closure() {
                        if closure.function.arity != arg_count as usize {
                            return self.runtime_error(
                                format!(
                                    "Expected {} arguments but got {}.",
                                    closure.function.arity, arg_count
                                )
                                .as_str(),
                            );
                        }

                        if self.frame_count == FRAMES_MAX {
                            return self.runtime_error("Stack overflow.");
                        }

                        self.add_frame(closure);
                    } else if let Some(native) = self.peek(arg_count).as_obj_native() {
                        let stack_before_arg = self.stack_count - arg_count as usize;
                        let arges = &self.stack[stack_before_arg..self.stack_count];
                        self.stack_count = stack_before_arg - 1; // 1 is for native fn itset
                        let res = native(arges);
                        self.push(res);
                    } else {
                        return self.runtime_error("Can only call functions and classes.");
                    }
                }
                OpClosure => {
                    let func = self.read_constant().as_obj_function().unwrap();
                    let mut upvalues = Vec::<ObjUpvalue>::with_capacity(func.upvalue_count);

                    for _ in 0..func.upvalue_count {
                        let upvalue_idx = self.read_byte() as usize;
                        let is_local = self.read_byte();
                        let upvalue = if is_local == 1 {
                            let v =
                                self.stack[self.current_frame().stack_offset + upvalue_idx].clone();
                            self.current_frame().capture_upvalue(upvalue_idx, v)
                        } else {
                            self.current_frame().get_upvalue(upvalue_idx)
                        };
                        upvalues.push(upvalue);
                    }
                    let closure = self.heap.allocate_closure(func, upvalues);
                    self.push(closure);
                }
                OpGetUpvalue => {
                    let idx = self.read_byte();
                    let upvalue = self.current_frame().get_upvalue(idx as _).borrow().clone();
                    self.push(upvalue);
                }
                OpSetUpvalue => {
                    let idx = self.read_byte();
                    let value = self.peek(0);
                    self.current_frame().set_upvalue(idx as _, value);
                }
                OpCloseUpvalue => {
                    self.close_upvalues();
                    self.pop();
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
                    let res: Value = self.heap.allocate_string(concated.as_str());
                    return Ok(res);
                }
                _ => return Err("Operands must be two numbers or two strings.".to_owned()),
            },
            _ => match instruction {
                OpAdd => return Err("Operands must be two numbers or two strings.".to_owned()),
                _ => return Err("Operands must be numbers.".to_owned()),
            },
        }
    }

    fn read_byte(&mut self) -> u8 {
        return self.current_frame().get_one();
    }

    fn read_constant(&mut self) -> Value {
        return self.current_frame().get_constant();
    }

    fn read_string(&mut self) -> Rc<ObjString> {
        return self.read_constant().cast_obj_string();
    }

    fn read_short(&mut self) -> usize {
        let b1 = self.read_byte();
        let b2 = self.read_byte();
        return ((b1 as usize) << 8) | (b2 as usize);
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_count] = value;
        self.stack_count += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_count -= 1;
        return self.stack[self.stack_count].clone();
    }

    fn peek(&mut self, delta: u8) -> Value {
        let offset = self.stack_count - 1 - delta as usize;
        return self.stack[offset].clone();
    }

    fn runtime_error(&mut self, msg: &str) -> InterpretResult {
        eprintln!("{}", msg);

        for i in (0..self.frame_count).rev() {
            let frame = self.frames[i].as_ref().unwrap();
            let name = match &frame.ty {
                FrameType::Function(closure) => format!("{}()", closure.function.name.chars),
                FrameType::Script(_) => "script".to_owned(),
            };
            let current_line = self.current_frame().get_line();

            eprintln!("[line {current_line}] in {name}");
        }

        return InterpretRuntimeError;
    }

    fn close_upvalues(&mut self) {
        let stack_baseline = self.current_frame().stack_offset;
        while let Some(inner) = self.current_frame().open_upvalues.clone() {
            let inner = inner.borrow();
            let upvalue_stack_idx = stack_baseline + inner.idx;
            if upvalue_stack_idx < self.stack_count - 1 {
                break;
            }

            *inner.upvalue.borrow_mut() = self.stack[upvalue_stack_idx].clone();
            self.current_frame().open_upvalues = inner.next.clone();
        }
    }
}

#[cfg(test)]
mod tests {

    use super::interpret;

    #[test]
    fn xxx() {
        interpret(
            r#"
            var f;
            var g;
            
            {
              var local = "local";
              fun f_() {
                print local;
                local = "after f";
                print local;
              }
              f = f_;
            
              fun g_() {
                print local;
                local = "after g";
                print local;
              }
              g = g_;
            }
            
            f();
            // expect: local
            // expect: after f
            
            g();
            // expect: after f
            // expect: after g            
        "#,
        );
    }
}
