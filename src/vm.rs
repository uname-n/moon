use std::collections::HashMap;
use std::fmt;

use crate::bytecode::Instruction;
use crate::value::Value;

#[derive(Debug, PartialEq)]
pub enum VMError {
    TypeError(String),
    DivisionByZero,
    StackUnderflow,
    UndefinedVariable(String),
    ReturnValue(Value),
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VMError::TypeError(msg) => write!(f, "Type error: {}", msg),
            VMError::DivisionByZero => write!(f, "Division by zero"),
            VMError::StackUnderflow => write!(f, "Stack underflow"),
            VMError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            VMError::ReturnValue(val) => write!(f, "Return: {}", val),
        }
    }
}

impl std::error::Error for VMError {}

struct CallFrame {
    pub code: Vec<Instruction>,
    pub ip: usize,
    pub constants: Vec<Value>,
    pub locals: Vec<Value>,
    pub _base: usize,
    pub closure: HashMap<String, Value>,
}

pub struct VM {
    pub globals: HashMap<String, Value>,

    call_stack: Vec<CallFrame>,

    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            call_stack: Vec::new(),
            stack: Vec::new(),
        }
    }

    pub fn define_global(&mut self, name: &str, value: Value) {
        self.globals.insert(name.to_string(), value);
    }

    fn pop_number(&mut self) -> Result<f64, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Number(n) => Ok(n),
            other => Err(VMError::TypeError(format!(
                "Expected number, got {}",
                other
            ))),
        }
    }

    fn pop_bool(&mut self) -> Result<bool, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Bool(b) => Ok(b),
            other => Err(VMError::TypeError(format!("Expected bool, got {}", other))),
        }
    }

    pub fn run(&mut self, code: &[Instruction], constants: &[Value]) -> Result<Value, VMError> {
        let mut ip = 0;
        while ip < code.len() {
            let instr = code[ip].clone();
            ip += 1;
            match instr {
                Instruction::LoadConst(idx) => {
                    let val = constants
                        .get(idx)
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
                    let n = self.pop_number()?;
                    self.stack.push(Value::Number(-n));
                }
                Instruction::Not => {
                    let b = self.pop_bool()?;
                    self.stack.push(Value::Bool(!b));
                }
                Instruction::Equal => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(Value::Bool(a == b));
                }
                Instruction::NotEqual => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(Value::Bool(a != b));
                }
                Instruction::Less => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Bool(a < b));
                }
                Instruction::Greater => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Bool(a > b));
                }
                Instruction::LessEqual => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Bool(a <= b));
                }
                Instruction::GreaterEqual => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Bool(a >= b));
                }
                Instruction::LoadLocal(_idx) => {
                    return Err(VMError::TypeError("LoadLocal in top-level".to_string()));
                }
                Instruction::StoreLocal(_idx) => {
                    return Err(VMError::TypeError("StoreLocal in top-level".to_string()));
                }
                Instruction::LoadGlobal(name) => {
                    let val = self
                        .globals
                        .get(&name)
                        .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                    self.stack.push(val.clone());
                }
                Instruction::StoreGlobal(name) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.globals.insert(name.clone(), val);
                }
                Instruction::LoadClosure(name) => {
                    if let Some(frame) = self.call_stack.last() {
                        let val = frame
                            .closure
                            .get(&name)
                            .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                        self.stack.push(val.clone());
                    } else {
                        return Err(VMError::UndefinedVariable(name));
                    }
                }
                Instruction::StoreClosure(name) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    if let Some(frame) = self.call_stack.last_mut() {
                        frame.closure.insert(name.clone(), val);
                    } else {
                        return Err(VMError::UndefinedVariable(name));
                    }
                }
                Instruction::Jump(target) => {
                    ip = target;
                }
                Instruction::JumpIfFalse(target) => {
                    let cond = self.pop_bool()?;
                    if !cond {
                        ip = target;
                    }
                }
                Instruction::JumpIfTrue(target) => {
                    let cond = self.pop_bool()?;
                    if cond {
                        ip = target;
                    }
                }
                Instruction::Call(_func_idx, arg_count) => {
                    let func_val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    match func_val {
                        Value::Function(mut func) => {
                            let mut args = Vec::new();
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                            }
                            args.reverse();
                            let closure = if let Some(c) = func.closure.take() {
                                c
                            } else if let Some(frame) = self.call_stack.last() {
                                frame.closure.clone()
                            } else {
                                HashMap::new()
                            };
                            let frame = CallFrame {
                                code: func.code.clone(),
                                ip: 0,
                                constants: func.constants.clone(),
                                locals: args,
                                _base: func.base,
                                closure,
                            };
                            self.call_stack.push(frame);
                            let ret = self.run_frame()?;
                            self.stack.push(ret);
                        }
                        Value::BuiltinFunction(_, f) => {
                            let mut args = Vec::new();
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                            }
                            args.reverse();
                            let ret = f(&args)?;
                            self.stack.push(ret);
                        }
                        other => {
                            return Err(VMError::TypeError(format!(
                                "Call of non-function: {}",
                                other
                            )));
                        }
                    }
                }
                Instruction::Pop => {
                    self.stack.pop().ok_or(VMError::StackUnderflow)?;
                }
                Instruction::Return => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    return Err(VMError::ReturnValue(val));
                }
            }
        }
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }

    fn run_frame(&mut self) -> Result<Value, VMError> {
        loop {
            let instr = {
                let frame = self.call_stack.last_mut().ok_or(VMError::StackUnderflow)?;
                if frame.ip >= frame.code.len() {
                    return Err(VMError::StackUnderflow);
                }
                let i = frame.code[frame.ip].clone();
                frame.ip += 1;
                i
            };
            match instr {
                Instruction::LoadConst(idx) => {
                    let frame = self.call_stack.last().ok_or(VMError::StackUnderflow)?;
                    let val = frame
                        .constants
                        .get(idx)
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
                    let n = self.pop_number()?;
                    self.stack.push(Value::Number(-n));
                }
                Instruction::Not => {
                    let b = self.pop_bool()?;
                    self.stack.push(Value::Bool(!b));
                }
                Instruction::Equal => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(Value::Bool(a == b));
                }
                Instruction::NotEqual => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(Value::Bool(a != b));
                }
                Instruction::Less => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Bool(a < b));
                }
                Instruction::Greater => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Bool(a > b));
                }
                Instruction::LessEqual => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Bool(a <= b));
                }
                Instruction::GreaterEqual => {
                    let b = self.pop_number()?;
                    let a = self.pop_number()?;
                    self.stack.push(Value::Bool(a >= b));
                }
                Instruction::LoadLocal(idx) => {
                    let frame = self.call_stack.last().ok_or(VMError::StackUnderflow)?;
                    let val = frame
                        .locals
                        .get(idx)
                        .ok_or_else(|| VMError::UndefinedVariable(format!("local#{}", idx)))?
                        .clone();
                    self.stack.push(val);
                }
                Instruction::StoreLocal(idx) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let frame = self.call_stack.last_mut().ok_or(VMError::StackUnderflow)?;
                    if idx < frame.locals.len() {
                        frame.locals[idx] = val;
                    } else {
                        while frame.locals.len() < idx {
                            frame.locals.push(Value::Number(0.0));
                        }
                        frame.locals.push(val);
                    }
                }
                Instruction::LoadGlobal(name) => {
                    let val = self
                        .globals
                        .get(&name)
                        .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                    self.stack.push(val.clone());
                }
                Instruction::StoreGlobal(name) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.globals.insert(name.clone(), val);
                }
                Instruction::LoadClosure(name) => {
                    if let Some(frame) = self.call_stack.last() {
                        let val = frame
                            .closure
                            .get(&name)
                            .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                        self.stack.push(val.clone());
                    } else {
                        return Err(VMError::UndefinedVariable(name));
                    }
                }
                Instruction::StoreClosure(name) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    if let Some(frame) = self.call_stack.last_mut() {
                        frame.closure.insert(name.clone(), val);
                    } else {
                        return Err(VMError::UndefinedVariable(name));
                    }
                }
                Instruction::Jump(target) => {
                    let frame = self.call_stack.last_mut().ok_or(VMError::StackUnderflow)?;
                    frame.ip = target;
                    continue;
                }
                Instruction::JumpIfFalse(target) => {
                    let cond = self.pop_bool()?;
                    if !cond {
                        let frame = self.call_stack.last_mut().ok_or(VMError::StackUnderflow)?;
                        frame.ip = target;
                        continue;
                    }
                }
                Instruction::JumpIfTrue(target) => {
                    let cond = self.pop_bool()?;
                    if cond {
                        let frame = self.call_stack.last_mut().ok_or(VMError::StackUnderflow)?;
                        frame.ip = target;
                        continue;
                    }
                }
                Instruction::Call(_func_idx, arg_count) => {
                    let func_val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    match func_val {
                        Value::Function(mut func) => {
                            let mut args = Vec::new();
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                            }
                            args.reverse();
                            let closure = if let Some(c) = func.closure.take() {
                                c
                            } else if let Some(frame) = self.call_stack.last() {
                                frame.closure.clone()
                            } else {
                                HashMap::new()
                            };
                            let new_frame = CallFrame {
                                code: func.code.clone(),
                                ip: 0,
                                constants: func.constants.clone(),
                                locals: args,
                                _base: func.base,
                                closure,
                            };
                            self.call_stack.push(new_frame);
                            let ret = self.run_frame()?;
                            self.stack.push(ret);
                        }
                        Value::BuiltinFunction(_, builtin) => {
                            let mut args = Vec::new();
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                            }
                            args.reverse();
                            let ret = builtin(&args)?;
                            self.stack.push(ret);
                        }
                        other => {
                            return Err(VMError::TypeError(format!(
                                "Attempted to call non-function: {}",
                                other
                            )));
                        }
                    }
                }
                Instruction::Pop => {
                    self.stack.pop().ok_or(VMError::StackUnderflow)?;
                }
                Instruction::Return => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.call_stack.pop();
                    return Ok(val);
                }
            }
        }
    }
}
