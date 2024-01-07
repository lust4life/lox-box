#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    OpReturn,
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
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        return self as _;
    }
}
