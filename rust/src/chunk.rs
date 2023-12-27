use crate::{
    object::{Obj, ObjType},
    op::OpCode,
};
use std::{
    alloc,
    alloc::Layout,
    fmt::Display,
    ops::{Deref, Neg},
    ptr,
    rc::Rc,
};

pub struct Vec<T> {
    ptr: *mut T,
    pub count: usize,
    pub capacity: usize,
}

impl<T> Drop for Vec<T> {
    fn drop(&mut self) {
        if self.capacity != 0 {
            let layout = Layout::array::<T>(self.capacity).unwrap();
            unsafe { alloc::dealloc(self.ptr.cast(), layout) };
        }
    }
}

impl<T> Deref for Vec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.count) }
    }
}

impl<T> Vec<T> {
    pub fn new() -> Self {
        let ptr = ptr::NonNull::dangling().as_ptr();
        return Self {
            ptr: ptr,
            count: 0,
            capacity: 0,
        };
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let new_layout = Layout::array::<T>(capacity).unwrap();
        let new_ptr = unsafe { alloc::alloc(new_layout) };
        if new_ptr.is_null() {
            alloc::handle_alloc_error(new_layout)
        } else {
            return Self {
                ptr: new_ptr.cast(),
                count: 0,
                capacity: capacity,
            };
        }
    }

    fn push(&mut self, item: T) {
        if self.capacity <= self.count {
            self.grow_array();
        }
        unsafe {
            let end = self.ptr.add(self.count);
            end.write(item);
        }
        self.count += 1;
    }

    pub fn grow_capacity(&self) -> usize {
        if self.capacity < 8 {
            return 8;
        } else {
            return self.capacity * 2;
        };
    }

    pub fn grow_array(&mut self) {
        let old_capacity = self.capacity;
        let new_capacity = self.grow_capacity();
        let new_layout = Layout::array::<T>(new_capacity).unwrap();

        let new_ptr = if old_capacity == 0 {
            // allcate
            unsafe { alloc::alloc(new_layout) }
        } else {
            // relocate
            let old_layout = Layout::array::<T>(old_capacity).unwrap();
            unsafe { alloc::realloc(self.ptr.cast(), old_layout, new_layout.size()) }
        };
        if new_ptr.is_null() {
            alloc::handle_alloc_error(new_layout)
        } else {
            self.ptr = new_ptr.cast();
            self.capacity = new_capacity;
        }
    }

    pub fn read(&self, idx: usize) -> &mut T {
        unsafe { &mut *self.ptr.add(idx) }
    }

    pub fn write(&mut self, idx: usize, val: T, add_count: bool) {
        unsafe { self.ptr.add(idx).write(val) };
        if add_count {
            self.count += 1;
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    NIL,
    BOOL(bool),
    NUMBER(f64),
    OBJ(Rc<Obj>),
}

impl Value {
    pub fn cast_truthy(&self) -> bool {
        match self {
            Value::NIL => false,
            Value::BOOL(inner) => inner.to_owned(),
            _ => true,
        }
    }
}

impl Neg for Value {
    type Output = Result<Self, String>;

    fn neg(self) -> Self::Output {
        use Value::*;
        return match self {
            NUMBER(inner) => Ok(NUMBER(-inner)),
            _ => Err("Operand must be a number.".to_owned()),
        };
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.to_owned() {
            Value::NIL => f.write_str("nil"),
            Value::BOOL(inner) => {
                if inner {
                    f.write_str("true")
                } else {
                    f.write_str("false")
                }
            }
            Value::NUMBER(inner) => write!(f, "{}", inner),
            Value::OBJ(obj) => match obj.ty {
                ObjType::ObjString(ref inner) => f.write_str(&inner.chars),
            },
        }
    }
}

pub struct Chunk {
    code: Vec<u8>,
    lines: Vec<usize>,
    contants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            contants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write_chunk(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    #[cfg(test)]
    fn disassemble(&self, name: &str) {
        println!("== {name} ==");
        let mut offset = 0;
        while offset < self.code.count {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn code_count(&self) -> usize {
        self.code.count
    }

    pub fn get_one<T: Copy>(&self, offset: usize) -> T {
        let instruction = unsafe { *(self.code.ptr.add(offset) as *const T) };
        return instruction;
    }

    pub fn set_one(&self, offset: usize, byte: u8) {
        unsafe { self.code.ptr.add(offset).write(byte) };
    }
    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{offset:04} ");

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ")
        } else {
            let line = self.lines[offset];
            print!("{line:4} ")
        }

        let instruction = unsafe { &*(self.code.ptr.add(offset) as *const OpCode) };
        let delta = match instruction {
            OpCode::OpReturn => simple_instruction("OP_RETURN"),
            OpCode::OpConstant => self.constant_instruction("OP_CONSTANT", offset),
            OpCode::OpAdd => simple_instruction("OP_ADD"),
            OpCode::OpSubtract => simple_instruction("OP_SUBTRACT"),
            OpCode::OpMultiply => simple_instruction("OP_MULTIPLY"),
            OpCode::OpDivide => simple_instruction("OP_DIVIDE"),
            OpCode::OpNegate => simple_instruction("OP_NEGATE"),
            OpCode::OpNot => simple_instruction("OP_NOT"),
            OpCode::OpLess => simple_instruction("OP_LESS"),
            OpCode::OpGreater => simple_instruction("OP_GREATER"),
            OpCode::OpEqual => simple_instruction("OP_EQUAL"),
            OpCode::OpTrue => simple_instruction("OP_TRUE"),
            OpCode::OpFalse => simple_instruction("OP_FALSE"),
            OpCode::OpNil => simple_instruction("OP_NIL"),
            OpCode::OpPrint => simple_instruction("OP_PRINT"),
            OpCode::OpPop => simple_instruction("OP_POP"),
            OpCode::OpDefineGlobal => simple_instruction("OP_DEFINE_GLOBAL"),
            OpCode::OpGetGlobal => simple_instruction("OP_GET_GLOBAL"),
            OpCode::OpSetGlobal => simple_instruction("OP_SET_GLOBAL"),
            OpCode::OpGetLocal => self.byte_instruction("OP_GET_LOCAL", offset),
            OpCode::OpSetLocal => self.byte_instruction("OP_SET_LOCAL", offset),
            OpCode::OpJumpIfFalse => self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset),
            OpCode::OpJump => self.jump_instruction("OP_JUMP", 1, offset),
            OpCode::OpLoop => self.jump_instruction("OP_LOOP", -1, offset),
        };

        return offset + delta;

        fn simple_instruction(name: &str) -> usize {
            println!("{name}");
            return 1;
        }
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant_idx = self.code[offset + 1];
        let constant_value = &self.contants[constant_idx as usize];
        println!("{name:-16} {constant_idx:4} '{constant_value:.2}'");
        return 2;
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        let constant_idx = self.code[offset + 1];
        println!("{name:-16} {constant_idx:4}");
        return 2;
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.contants.push(value);
        return self.contants.count - 1;
    }

    pub fn get_constant(&self, offset: usize) -> Value {
        let constant_idx = self.code[offset];
        let constant_value = self.contants[constant_idx as usize].clone();
        return constant_value;
    }

    pub fn get_line(&self, offset: usize) -> usize {
        return self.lines[offset];
    }

    fn jump_instruction(&self, name: &str, sign: i32, offset: usize) -> usize {
        let delta = (self.code[offset + 1] as u16) << 8 | (self.code[offset + 2] as u16);
        println!(
            "{:-16} {:4} -> {}",
            name,
            offset,
            sign * (delta as i32) + (3 + offset) as i32
        );
        return 3;
    }
}

#[cfg(test)]
mod tests {

    use crate::vm::Heap;

    #[test]
    fn poc() {
        let mut heap = Heap::new();
        let a = heap.allocate_string("1");
        let b = heap.allocate_string("1");
        assert_eq!(a, b);
    }
}
