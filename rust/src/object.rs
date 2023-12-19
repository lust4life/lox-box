use std::rc::Rc;

#[derive(PartialEq)]
pub enum ObjType {
    ObjString(String),
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
        let ty = ObjType::ObjString(String::from(str));
        Self { ty, next }
    }
}

// pub struct ObjString {
//     pub obj: Obj,
//     pub str: String,
// }
