use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

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
    pub code: Arc<Vec<Instruction>>,
    pub ip: usize,
    pub constants: Arc<Vec<Value>>,
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

    #[inline(always)]
    fn pop_number(&mut self) -> Result<f64, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Number(n) => Ok(n),
            other => Err(VMError::TypeError(format!("Expected number, got {}", other))),
        }
    }

    #[inline(always)]
    fn pop_bool(&mut self) -> Result<bool, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Bool(b) => Ok(b),
            other => Err(VMError::TypeError(format!("Expected bool, got {}", other))),
        }
    }

    pub fn run(&mut self, code: &[Instruction], constants: &[Value]) -> Result<Value, VMError> {
        self.call_stack.push(CallFrame {
            code: Arc::new(code.to_vec()),
            ip: 0,
            constants: Arc::new(constants.to_vec()),
            locals: Vec::new(),
            _base: 0,
            closure: HashMap::new(),
        });

        loop {
            if self.call_stack.is_empty() {
                break;
            }
            let instr = {
                let frame_index = self.call_stack.len() - 1;
                let frame = &mut self.call_stack[frame_index];
                if frame.ip >= frame.code.len() {
                    let ret = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.call_stack.pop();
                    if self.call_stack.is_empty() {
                        return Ok(ret);
                    } else {
                        self.stack.push(ret);
                        continue;
                    }
                }
                let ip = frame.ip;
                frame.ip += 1;
                frame.code[ip].clone()
            };

            match instr {
                Instruction::LoadConst(idx) => {
                    let frame_index = self.call_stack.len() - 1;
                    let frame = &self.call_stack[frame_index];
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
                    let frame_index = self.call_stack.len() - 1;
                    let frame = &self.call_stack[frame_index];
                    let val = frame.locals.get(idx).ok_or_else(|| VMError::UndefinedVariable(format!("local#{}", idx)))?.clone();
                    self.stack.push(val);
                }
                Instruction::StoreLocal(idx) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let frame_index = self.call_stack.len() - 1;
                    let frame = &mut self.call_stack[frame_index];
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
                    let val = self.globals.get(&name)
                        .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?
                        .clone();
                    self.stack.push(val);
                }
                Instruction::StoreGlobal(name) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.globals.insert(name, val);
                }
                Instruction::LoadClosure(name) => {
                    let frame_index = self.call_stack.len() - 1;
                    let frame = &self.call_stack[frame_index];
                    let val = frame.closure.get(&name)
                        .cloned()
                        .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                    self.stack.push(val);
                }
                Instruction::StoreClosure(name) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let frame_index = self.call_stack.len() - 1;
                    let frame = &mut self.call_stack[frame_index];
                    frame.closure.insert(name, val);
                }
                Instruction::Jump(target) => {
                    let frame_index = self.call_stack.len() - 1;
                    let frame = &mut self.call_stack[frame_index];
                    frame.ip = target;
                }
                Instruction::JumpIfFalse(target) => {
                    let cond = self.pop_bool()?;
                    if !cond {
                        let frame_index = self.call_stack.len() - 1;
                        let frame = &mut self.call_stack[frame_index];
                        frame.ip = target;
                    }
                }
                Instruction::JumpIfTrue(target) => {
                    let cond = self.pop_bool()?;
                    if cond {
                        let frame_index = self.call_stack.len() - 1;
                        let frame = &mut self.call_stack[frame_index];
                        frame.ip = target;
                    }
                }
                Instruction::Call(_, arg_count) => {
                    if self.stack.len() < arg_count + 1 {
                        return Err(VMError::StackUnderflow);
                    }
                    let func_val = self.stack.remove(self.stack.len() - arg_count - 1);
                    let args = self.stack.split_off(self.stack.len() - arg_count);
                    match func_val {
                        Value::Function(func) => {
                            let parent_closure = {
                                let frame_index = self.call_stack.len() - 1;
                                let frame = &self.call_stack[frame_index];
                                frame.closure.clone()
                            };
                            let closure = if let Some(c) = func.closure {
                                c
                            } else if parent_closure.is_empty() {
                                HashMap::new()
                            } else {
                                parent_closure
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
                        }
                        Value::BuiltinFunction(_, builtin_fn) => {
                            let ret = builtin_fn(&args)?;
                            self.stack.push(ret);
                        }
                        other => {
                            return Err(VMError::TypeError(format!("Attempted to call non-function: {}", other)));
                        }
                    }
                }
                Instruction::TailCall(_, arg_count) => {
                    if self.stack.len() < arg_count + 1 {
                        return Err(VMError::StackUnderflow);
                    }
                    let func_val = self.stack.remove(self.stack.len() - arg_count - 1);
                    let args = self.stack.split_off(self.stack.len() - arg_count);
                    match func_val {
                        Value::Function(func) => {
                            let parent_closure = {
                                let frame_index = self.call_stack.len() - 1;
                                let frame = &self.call_stack[frame_index];
                                frame.closure.clone()
                            };
                            let closure = if let Some(c) = func.closure {
                                c
                            } else if parent_closure.is_empty() {
                                HashMap::new()
                            } else {
                                parent_closure
                            };
                            let current_frame_index = self.call_stack.len() - 1;
                            self.call_stack[current_frame_index] = CallFrame {
                                code: func.code.clone(),
                                ip: 0,
                                constants: func.constants.clone(),
                                locals: args,
                                _base: func.base,
                                closure,
                            };
                        }
                        Value::BuiltinFunction(_, builtin_fn) => {
                            let ret = builtin_fn(&args)?;
                            self.stack.push(ret.clone());
                            self.call_stack.pop();
                            if self.call_stack.is_empty() {
                                return Ok(ret);
                            } else {
                                self.stack.push(ret);
                            }
                        }
                        other => {
                            return Err(VMError::TypeError(format!("Attempted to tail call non-function: {}", other)));
                        }
                    }
                }
                Instruction::Pop => {
                    self.stack.pop().ok_or(VMError::StackUnderflow)?;
                }
                Instruction::Return => {
                    let ret_val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.call_stack.pop();
                    if self.call_stack.is_empty() {
                        return Ok(ret_val);
                    } else {
                        self.stack.push(ret_val);
                    }
                }
            }
        }
        Err(VMError::StackUnderflow)
    }
}