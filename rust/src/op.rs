#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    OpReturn = 0,
    OpConstant,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
    OpNot,
    OpLess,
    OpGreater,
    OpEqual,
    OpTrue,
    OpFalse,
    OpNil,
    OpPrint,
    OpPop,
    OpDefineGlobal,
    OpGetGlobal,
    OpSetGlobal,
    OpGetLocal,
    OpSetLocal,
    OpJumpIfFalse,
    OpJump,
    OpLoop,
    OpCall,
    OpClosure,
    OpGetUpvalue,
    OpSetUpvalue,
    OpCloseUpvalue,
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        return self as _;
    }
}
