use std::{
    fmt::{Debug, Display},
    ptr,
    rc::Rc,
};

use crate::chunk::Chunk;

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
    chunk: Chunk,
    name: Rc<ObjString>,
    arity: usize,
}

impl ObjFunction {
    pub fn new(name: Rc<ObjString>, arity: usize) -> Self {
        Self {
            chunk: Chunk::new(),
            name,
            arity,
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

#[derive(Debug, PartialEq)]
pub enum ObjType {
    ObjString(Rc<ObjString>),
    ObjFunction(Rc<ObjFunction>),
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
        }
    }
}

impl Obj {
    pub fn from_obj_string(obj_str: Rc<ObjString>, next: Option<Rc<Obj>>) -> Self {
        let ty = ObjType::ObjString(obj_str);
        Self { ty, next }
    }

    pub fn cast_obj_string(&self) -> Rc<ObjString> {
        match &self.ty {
            ObjType::ObjString(inner) => inner.clone(),
            _ => panic!("expected to be a string"),
        }
    }
}
