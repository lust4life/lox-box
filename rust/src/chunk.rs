use std::{alloc, alloc::Layout, any::type_name, ops::Deref, ptr};

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    OpReturn,
    OpConstant,
}

struct Vec<T> {
    ptr: *mut T,
    count: usize,
    capacity: usize,
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
    fn new() -> Self {
        let ptr = ptr::NonNull::dangling().as_ptr();
        return Self {
            ptr: ptr,
            count: 0,
            capacity: 0,
        };
    }

    fn write_item(&mut self, item: T) {
        if self.capacity <= self.count {
            self.grow_array();
        }
        unsafe {
            let end = self.ptr.add(self.count);
            ptr::write(end, item);
        }
        self.count += 1;
    }

    fn grow_capacity(&self) -> usize {
        if self.capacity < 8 {
            return 8;
        } else {
            return self.capacity * 2;
        };
    }

    fn grow_array(&mut self) {
        let old_capacity = self.capacity;
        let new_capacity = self.grow_capacity();
        let new_layout = Layout::array::<u8>(new_capacity).unwrap();

        let new_ptr = if old_capacity == 0 {
            // allcate
            unsafe { alloc::alloc(new_layout) }
        } else {
            // relocate
            let old_layout = Layout::array::<u8>(old_capacity).unwrap();
            unsafe { alloc::realloc(self.ptr.cast(), old_layout, new_layout.size()) }
        };
        if new_ptr.is_null() {
            alloc::handle_alloc_error(new_layout)
        } else {
            self.ptr = new_ptr.cast();
            self.capacity = new_capacity;
        }
    }
}

type Value = f64;
struct Chunk {
    code: Vec<u8>,
    lines: Vec<i32>,
    contants: Vec<Value>,
}

impl Chunk {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            contants: Vec::new(),
            lines: Vec::new(),
        }
    }

    fn write_chunk(&mut self, byte: u8, line: i32) {
        self.code.write_item(byte);
        self.lines.write_item(line);
    }

    fn disassemble(&self, name: &str) {
        println!("== {name} ==");
        let mut offset = 0;
        while offset < self.code.count {
            offset = self.disassemble_instruction(offset);
        }
    }

    fn disassemble_instruction(&self, offset: usize) -> usize {
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
            _ => {
                println!("Unknow opcode {instruction:?}");
                return 1;
            }
        };

        return offset + delta;

        fn simple_instruction(name: &str) -> usize {
            println!("{name}");
            return 1;
        }
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant_idx = unsafe { *self.code.ptr.add(offset + 1) };
        let constant_value = unsafe { *self.contants.ptr.add(constant_idx as _) };
        println!("{name:-16} {constant_idx:4} '{constant_value}'");
        return 2;
    }

    fn add_constant(&mut self, value: Value) -> usize {
        self.contants.write_item(value);
        return self.contants.count - 1;
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn poc() {
        let mut chunk = Chunk::new();
        let idx = chunk.add_constant(1.2);
        chunk.write_chunk(OpCode::OpConstant as u8, 123);
        chunk.write_chunk(idx as _, 123);
        chunk.write_chunk(OpCode::OpReturn as u8, 123);
        chunk.disassemble("test chunk");
    }
}
