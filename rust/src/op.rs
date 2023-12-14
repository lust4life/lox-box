#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    OpReturn,
    OpConstant,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        return self as _;
    }
}
