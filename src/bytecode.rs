#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst(usize),
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    Not,
}
