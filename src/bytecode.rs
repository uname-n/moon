#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    LoadConst(usize),
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    Not,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    LoadLocal(usize),
    StoreLocal(usize),
    LoadGlobal(String),
    StoreGlobal(String),
    LoadClosure(String),
    StoreClosure(String),
    Jump(usize),
    JumpIfFalse(usize),
    JumpIfTrue(usize),
    Call(usize, usize),
    TailCall(usize, usize),
    Return,
    Pop,
}