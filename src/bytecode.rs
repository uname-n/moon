// src/bytecode.rs

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // push a constant onto the stack
    LoadConst(usize),
    // arithmetic operations
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    Not,
    // equality operator
    Equal,
    // variable access: the operand is the slot index for the variable
    LoadVar(usize),
    StoreVar(usize),
    // unconditional jump to instruction index
    Jump(usize),
    // jump if top of stack is false (pop the boolean)
    JumpIfFalse(usize),
    // call a function – the constant table holds the Function object.
    // The operands are (function constant index, number of arguments)
    Call(usize, usize),
    // return from function; optionally returns a value on the stack.
    Return,
    // discard top-of-stack value
    Pop,
}
