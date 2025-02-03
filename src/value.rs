use crate::bytecode::Instruction;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Function(Function),
    BuiltinFunction(String, fn(&[Value]) -> Result<Value, crate::vm::VMError>),
    Str(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Function(func) => write!(f, "<fn {}>", func.name),
            Value::BuiltinFunction(name, _) => write!(f, "<builtin {}>", name),
            Value::Str(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub code: Arc<Vec<Instruction>>,
    pub constants: Arc<Vec<Value>>,
    pub base: usize,
    pub closure: Option<HashMap<String, Value>>,
}