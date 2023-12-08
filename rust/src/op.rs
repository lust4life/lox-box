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
