use std::{ptr, rc::Rc};

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

#[derive(PartialEq, Debug)]
pub enum ObjType {
    ObjString(ObjString),
}

impl ObjType {
    pub fn cast_string(&self) -> &ObjString {
        match self {
            ObjType::ObjString(inner) => inner,
        }
    }
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

impl Obj {
    pub fn new_string(obj_str: ObjString, next: Option<Rc<Obj>>) -> Self {
        let ty = ObjType::ObjString(obj_str);
        Self { ty, next }
    }
}
