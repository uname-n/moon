#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    LoadConst(usize),

    // Arithmetic and unary ops
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

    // Local variable access by numeric slot
    LoadLocal(usize),
    StoreLocal(usize),

    // Global variable access by name
    LoadGlobal(String),
    StoreGlobal(String),

    // Closure variable access by name
    LoadClosure(String),
    StoreClosure(String),

    // Control flow
    Jump(usize),
    JumpIfFalse(usize),
    JumpIfTrue(usize),

    // Function call & return
    Call(usize, usize),
    Return,

    // Discard top-of-stack
    Pop,
}