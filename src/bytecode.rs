// src/bytecode.rs

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Push a constant onto the stack
    LoadConst(usize),

    // Arithmetic and unary ops
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    Not,
    Equal,

    // Local variable access by numeric slot
    LoadLocal(usize),
    StoreLocal(usize),

    // Global variable access by name
    LoadGlobal(String),
    StoreGlobal(String),

    // Control flow
    Jump(usize),
    JumpIfFalse(usize),

    // Function call & return
    // (Function object is typically in the constants array, or could be on stack)
    Call(usize, usize),
    Return,

    // Discard top-of-stack
    Pop,
}
