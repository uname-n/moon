use crate::bytecode::Instruction;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Function(Function),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Function(func) => write!(f, "<fn {}>", func.name),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub code: Vec<Instruction>,
    pub constants: Vec<Value>,
    pub base: usize,
}