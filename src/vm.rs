use crate::bytecode::Instruction;
use crate::value::Value;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum VMError {
    TypeError(String),
    DivisionByZero,
    StackUnderflow,
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VMError::TypeError(msg) => write!(f, "Type error: {}", msg),
            VMError::DivisionByZero => write!(f, "Division by zero"),
            VMError::StackUnderflow => write!(f, "Stack underflow"),
        }
    }
}

impl std::error::Error for VMError {}

pub struct VM {
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    /// Pops a number from the stack or returns an error.
    fn pop_number(&mut self) -> Result<f64, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Number(n) => Ok(n),
            _ => Err(VMError::TypeError("Expected a number".into())),
        }
    }

    /// Pops a boolean from the stack or returns an error.
    fn pop_bool(&mut self) -> Result<bool, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Bool(b) => Ok(b),
            _ => Err(VMError::TypeError("Expected a boolean".into())),
        }
    }

    pub fn run(&mut self, code: &[Instruction], constants: &[Value]) -> Result<Value, VMError> {
        let mut ip = 0;
        while ip < code.len() {
            match code[ip] {
                Instruction::LoadConst(idx) => {
                    let val = constants.get(idx)
                        .ok_or_else(|| VMError::TypeError(format!("No constant at index {}", idx)))?
                        .clone();
                    self.stack.push(val);
                }
                Instruction::Add => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Number(a + b));
                }
                Instruction::Sub => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Number(a - b));
                }
                Instruction::Mul => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Number(a * b));
                }
                Instruction::Div => {
                    let b = self.pop_number()?;
                    if b == 0.0 {
                        return Err(VMError::DivisionByZero);
                    }
                    let a = self.pop_number()?;
                    self.stack.push(Value::Number(a / b));
                }
                Instruction::Negate => {
                    let a = self.pop_number()?;
                    self.stack.push(Value::Number(-a));
                }
                Instruction::Not => {
                    let b = self.pop_bool()?;
                    self.stack.push(Value::Bool(!b));
                }
            }
            ip += 1;
        }
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }
}
