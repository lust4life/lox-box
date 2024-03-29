use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    ptr,
    rc::Rc,
};

use crate::chunk::{Chunk, Value, Vec};

#[derive(Debug, Clone)]
pub struct ObjString {
    pub chars: String,
    pub hash: usize,
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        return ptr::eq(self, other);
    }
}

impl ObjString {
    pub fn hash_string(str: &str) -> usize {
        let mut hash = 2166136261usize;
        for i in str.as_bytes() {
            hash ^= *i as usize;
            hash = hash.wrapping_mul(16777619);
        }
        return hash;
    }

    pub fn new(chars: &str) -> Self {
        let hash = Self::hash_string(chars);
        Self {
            chars: chars.to_string(),
            hash,
        }
    }
}

pub struct ObjFunction {
    pub chunk: Chunk,
    pub name: Rc<ObjString>,
    pub arity: usize,
    pub upvalue_count: usize,
}

impl ObjFunction {
    pub fn new(name: Rc<ObjString>) -> Self {
        Self {
            chunk: Chunk::new(),
            name,
            arity: 0,
            upvalue_count: 0,
        }
    }
}

impl PartialEq for ObjFunction {
    fn eq(&self, other: &Self) -> bool {
        return ptr::eq(self, other);
    }
}

impl Debug for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ObjFunction")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .finish()
    }
}

pub type ObjNative = fn(args: &[Value]) -> Value;

pub struct OpenUpvaule {
    pub idx: usize,
    pub upvalue: ObjUpvalue,
    pub next: Option<Rc<RefCell<OpenUpvaule>>>,
}

impl OpenUpvaule {
    pub fn new(idx: usize, next: Option<Rc<RefCell<OpenUpvaule>>>) -> Self {
        Self {
            idx,
            upvalue: Rc::new(RefCell::new(Upvalue::OnStack(idx))),
            next,
        }
    }
}

pub enum Upvalue {
    OnStack(usize),
    OnHeap(Value),
}
pub type ObjUpvalue = Rc<RefCell<Upvalue>>;

pub struct ObjClosure {
    pub function: Rc<ObjFunction>,
    pub upvalues: Vec<ObjUpvalue>,
}

impl PartialEq for ObjClosure {
    fn eq(&self, other: &Self) -> bool {
        return ptr::eq(self, other);
    }
}

impl Debug for ObjClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ObjClosure")
            .field("function", &self.function)
            .field("upvalues.count", &self.upvalues.count)
            .finish()
    }
}

#[derive(Debug, PartialEq)]
pub enum ObjType {
    ObjString(Rc<ObjString>),
    ObjFunction(Rc<ObjFunction>),
    ObjNative(ObjNative),
    ObjClosure(Rc<ObjClosure>),
}

#[derive(Debug)]
pub struct Obj {
    pub ty: ObjType,
    pub next: Option<Rc<Obj>>,
}

impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            ObjType::ObjString(inner) => f.write_str(&inner.chars),
            ObjType::ObjFunction(inner) => write!(f, "<fn {}>", inner.name.chars),
            ObjType::ObjNative(_) => f.write_str("<native fn>"),
            ObjType::ObjClosure(closure) => write!(f, "<fn {}>", closure.function.name.chars),
        }
    }
}
