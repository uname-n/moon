#![allow(clippy::unnecessary_wraps)]
use crate::bytecode::Instruction;
use crate::value::Value;
use std::fmt;

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
    pub base: usize,
}

pub struct VM {
    globals: Vec<Value>,
    call_stack: Vec<CallFrame>,
    stack: Vec<Value>,
}

impl VM {
    pub fn new(globals: Vec<Value>) -> Self {
        Self {
            globals,
            call_stack: Vec::new(),
            stack: Vec::new(),
        }
    }
    
    fn pop_number(&mut self) -> Result<f64, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Number(n) => Ok(n),
            _ => Err(VMError::TypeError("Expected a number".into())),
        }
    }
    
    fn pop_bool(&mut self) -> Result<bool, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Bool(b) => Ok(b),
            _ => Err(VMError::TypeError("Expected a boolean".into())),
        }
    }
    
    pub fn run(&mut self, code: &[Instruction], constants: &[Value]) -> Result<Value, VMError> {
        let mut ip = 0;
        while ip < code.len() {
            match &code[ip] {
                Instruction::LoadConst(idx) => {
                    let val = constants.get(*idx)
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
                    if b == 0.0 { return Err(VMError::DivisionByZero); }
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
                Instruction::Equal => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(Value::Bool(a == b));
                }
                Instruction::LoadVar(var_index) => {
                    let val = self.globals.get(*var_index)
                        .ok_or_else(|| VMError::UndefinedVariable(format!("var at slot {}", var_index)))?
                        .clone();
                    self.stack.push(val);
                }
                Instruction::StoreVar(var_index) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    if *var_index < self.globals.len() {
                        self.globals[*var_index] = val;
                    } else {
                        self.globals.push(val);
                    }
                }
                Instruction::Jump(target) => {
                    ip = *target;
                    continue;
                }
                Instruction::JumpIfFalse(target) => {
                    let cond = self.pop_bool()?;
                    if !cond {
                        ip = *target;
                        continue;
                    }
                }
                Instruction::Call(_func_const_idx, arg_count) => {
                    let func_val = self.stack.pop().expect("Expected function on stack");
                    if let Value::Function(func) = func_val {
                        let mut args = Vec::new();
                        for _ in 0..*arg_count {
                            args.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                        }
                        args.reverse();
                        let frame = CallFrame {
                            code: func.code.clone(),
                            ip: 0,
                            constants: func.constants.clone(),
                            locals: args,
                            base: func.base,
                        };
                        self.call_stack.push(frame);
                        let ret = self.run_frame()?;
                        self.stack.push(ret);
                    } else {
                        return Err(VMError::TypeError("Attempted to call a non-function".into()));
                    }
                }
                Instruction::Return => {
                    let ret = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    return Err(VMError::ReturnValue(ret));
                }
                Instruction::Pop => { self.stack.pop().ok_or(VMError::StackUnderflow)?; }
            }
            ip += 1;
        }
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }
    
    fn run_frame(&mut self) -> Result<Value, VMError> {
        loop {
            let instr = {
                let frame = self.call_stack.last_mut().unwrap();
                if frame.ip >= frame.code.len() {
                    return Err(VMError::StackUnderflow);
                }
                let instr = frame.code[frame.ip].clone();
                frame.ip += 1;
                instr
            };
            match instr {
                Instruction::LoadConst(idx) => {
                    let constant = {
                        let frame = self.call_stack.last().unwrap();
                        frame.constants.get(idx)
                            .cloned()
                            .ok_or_else(|| VMError::TypeError(format!("No constant at index {}", idx)))?
                    };
                    self.stack.push(constant);
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
                Instruction::Equal => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(Value::Bool(a == b));
                }
                Instruction::LoadVar(idx) => {
                    let frame = self.call_stack.last().unwrap();
                    let value = if idx < frame.base {
                        self.globals.get(idx)
                            .ok_or_else(|| VMError::UndefinedVariable(format!("No global variable at slot {}", idx)))?
                            .clone()
                    } else {
                        let local_index = idx - frame.base;
                        if local_index < frame.locals.len() {
                            frame.locals[local_index].clone()
                        } else {
                            return Err(VMError::UndefinedVariable(format!("No local variable at slot {}", idx)));
                        }
                    };
                    self.stack.push(value);
                }
                Instruction::StoreVar(idx) => {
                    let value = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let frame = self.call_stack.last_mut().unwrap();
                    if idx < frame.base {
                        if idx < self.globals.len() {
                            self.globals[idx] = value;
                        } else {
                            while self.globals.len() <= idx {
                                self.globals.push(Value::Number(0.0));
                            }
                            self.globals[idx] = value;
                        }
                    } else {
                        let local_index = idx - frame.base;
                        if local_index < frame.locals.len() {
                            frame.locals[local_index] = value;
                        } else if local_index == frame.locals.len() {
                            frame.locals.push(value);
                        } else {
                            while frame.locals.len() < local_index {
                                frame.locals.push(Value::Number(0.0));
                            }
                            frame.locals.push(value);
                        }
                    }
                }
                Instruction::Jump(target) => {
                    let frame = self.call_stack.last_mut().unwrap();
                    frame.ip = target;
                    continue;
                }
                Instruction::JumpIfFalse(target) => {
                    let cond = self.pop_bool()?;
                    if !cond {
                        let frame = self.call_stack.last_mut().unwrap();
                        frame.ip = target;
                        continue;
                    }
                }
                Instruction::Call(_func_const_idx, arg_count) => {
                    let func_val = self.stack.pop().expect("Expected function on stack");
                    if let Value::Function(func) = func_val {
                        let mut args = Vec::new();
                        for _ in 0..arg_count {
                            args.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                        }
                        args.reverse();
                        let new_frame = CallFrame {
                            code: func.code.clone(),
                            ip: 0,
                            constants: func.constants.clone(),
                            locals: args,
                            base: func.base,
                        };
                        self.call_stack.push(new_frame);
                        let ret = self.run_frame()?;
                        self.stack.push(ret);
                    } else {
                        return Err(VMError::TypeError("Attempted to call a non-function".into()));
                    }
                }
                Instruction::Pop => {
                    self.stack.pop().ok_or(VMError::StackUnderflow)?;
                }
                Instruction::Return => {
                    let ret = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.call_stack.pop();
                    return Ok(ret);
                }
            }
        }
    }
}