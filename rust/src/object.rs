use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub struct ObjString {
    pub chars: String,
    pub hash: usize,
}

impl ObjString {
    fn hash_string(str: &str) -> usize {
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

#[derive(PartialEq)]
pub enum ObjType {
    ObjString(ObjString),
}

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
    pub fn new_string(str: &str, next: Option<Rc<Obj>>) -> Self {
        let ty = ObjType::ObjString(ObjString::new(str));
        Self { ty, next }
    }
}
