use crate::bytecode::Instruction;
use std::collections::HashMap;
use std::sync::Arc;
use num_bigint::BigInt;
use num_traits::{Zero, One, ToPrimitive};

#[derive(Clone, Debug)]
pub enum Value {
    Integer(BigInt),
    Float(f64),
    Bool(bool),
    Function(Function),
    BuiltinFunction(String, fn(&[Value]) -> Result<Value, crate::vm::VMError>),
    Str(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Function(func) => write!(f, "<fn {}>", func.name),
            Value::BuiltinFunction(name, _) => write!(f, "<builtin {}>", name),
            Value::Str(s) => write!(f, "{}", s),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Integer(a), Value::Float(b)) => a.to_f64().map_or(false, |v| v == *b),
            (Value::Float(a), Value::Integer(b)) => b.to_f64().map_or(false, |v| v == *a),
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Str(s1), Value::Str(s2)) => s1 == s2,
            (Value::Function(f1), Value::Function(f2)) => f1.name == f2.name,
            (Value::BuiltinFunction(n1, _), Value::BuiltinFunction(n2, _)) => n1 == n2,
            _ => false,
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