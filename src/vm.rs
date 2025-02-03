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

pub struct CallFrame {
    pub code: Arc<Vec<Instruction>>,
    pub ip: usize,
    pub constants: Arc<Vec<Value>>,
    pub locals: Vec<Value>,
    pub _base: usize,
    pub closure: HashMap<String, Value>,
}

pub struct VM {
    pub globals: HashMap<String, Value>,
    pub call_stack: Vec<CallFrame>,
    pub stack: Vec<Value>,
    pub sp: usize,
}
impl VM {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            call_stack: Vec::new(),
            stack: Vec::with_capacity(1024),
            sp: 0,
        }
    }
    pub fn define_global(&mut self, name: &str, value: Value) {
        self.globals.insert(name.to_string(), value);
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
            // Execute instructions for the top frame until a control transfer occurs.
            let transferred = {
                let frame = self.call_stack.last_mut().unwrap();
                let mut control_transferred = false;
                while frame.ip < frame.code.len() {
                    let instr = unsafe { frame.code.get_unchecked(frame.ip) }.clone();
                    frame.ip += 1;
                    match instr {
                        Instruction::LoadConst(idx) => {
                            let val = frame.constants.get(idx)
                                .ok_or_else(|| VMError::TypeError(format!("No constant at index {}", idx)))?.clone();
                            self.stack.push(val);
                        }
                        Instruction::Add => {
                            let b = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            let a = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            self.stack.push(Value::Number(a + b));
                        }
                        Instruction::Sub => {
                            let b = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            let a = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            self.stack.push(Value::Number(a - b));
                        }
                        Instruction::Mul => {
                            let b = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            let a = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            self.stack.push(Value::Number(a * b));
                        }
                        Instruction::Div => {
                            let b = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            if b == 0.0 {
                                return Err(VMError::DivisionByZero);
                            }
                            let a = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            self.stack.push(Value::Number(a / b));
                        }
                        Instruction::Negate => {
                            let n = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            self.stack.push(Value::Number(-n));
                        }
                        Instruction::Not => {
                            let b = match self.stack.pop() {
                                Some(Value::Bool(b)) => b,
                                Some(other) => return Err(VMError::TypeError(format!("Expected bool, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
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
                            let b = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            let a = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            self.stack.push(Value::Bool(a < b));
                        }
                        Instruction::Greater => {
                            let b = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            let a = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            self.stack.push(Value::Bool(a > b));
                        }
                        Instruction::LessEqual => {
                            let b = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            let a = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            self.stack.push(Value::Bool(a <= b));
                        }
                        Instruction::GreaterEqual => {
                            let b = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            let a = match self.stack.pop() {
                                Some(Value::Number(n)) => n,
                                Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            self.stack.push(Value::Bool(a >= b));
                        }
                        Instruction::LoadLocal(idx) => {
                            let val = frame.locals.get(idx)
                                .ok_or_else(|| VMError::UndefinedVariable(format!("local#{}", idx)))?.clone();
                            self.stack.push(val);
                        }
                        Instruction::StoreLocal(idx) => {
                            let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                            if idx < frame.locals.len() {
                                frame.locals[idx] = val;
                            } else {
                                while frame.locals.len() < idx {
                                    frame.locals.push(Value::Number(0.0));
                                }
                                frame.locals.push(val);
                            }
                        }
                        Instruction::LoadGlobal(ref name) => {
                            let val = self.globals.get(name)
                                .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?.clone();
                            self.stack.push(val);
                        }
                        Instruction::StoreGlobal(ref name) => {
                            let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                            self.globals.insert(name.clone(), val);
                        }
                        Instruction::LoadClosure(ref name) => {
                            let val = frame.closure.get(name)
                                .cloned()
                                .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                            self.stack.push(val);
                        }
                        Instruction::StoreClosure(ref name) => {
                            let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                            frame.closure.insert(name.clone(), val);
                        }
                        Instruction::Jump(target) => {
                            frame.ip = target;
                        }
                        Instruction::JumpIfFalse(target) => {
                            let cond = match self.stack.pop() {
                                Some(Value::Bool(b)) => b,
                                Some(other) => return Err(VMError::TypeError(format!("Expected bool, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            if !cond {
                                frame.ip = target;
                            }
                        }
                        Instruction::JumpIfTrue(target) => {
                            let cond = match self.stack.pop() {
                                Some(Value::Bool(b)) => b,
                                Some(other) => return Err(VMError::TypeError(format!("Expected bool, got {}", other))),
                                None => return Err(VMError::StackUnderflow),
                            };
                            if cond {
                                frame.ip = target;
                            }
                        }
                        Instruction::Call(_, arg_count) => {
                            if self.stack.len() < arg_count + 1 {
                                return Err(VMError::StackUnderflow);
                            }
                            let func_index = self.stack.len() - arg_count - 1;
                            let func_val = self.stack.remove(func_index);
                            let args: Vec<Value> = self.stack.drain(self.stack.len()-arg_count..).collect();
                            match func_val {
                                Value::Function(ref func) => {
                                    let new_frame = CallFrame {
                                        code: func.code.clone(),
                                        ip: 0,
                                        constants: func.constants.clone(),
                                        locals: args,
                                        _base: func.base,
                                        closure: func.closure.clone().unwrap_or_else(|| frame.closure.clone()),
                                    };
                                    self.call_stack.push(new_frame);
                                    control_transferred = true;
                                    break;
                                }
                                Value::BuiltinFunction(_, builtin_fn) => {
                                    let ret = builtin_fn(&args)?;
                                    self.stack.push(ret);
                                    control_transferred = true;
                                    break;
                                }
                                other => return Err(VMError::TypeError(format!("Attempted to call non-function: {}", other))),
                            }
                        }
                        Instruction::TailCall(_, arg_count) => {
                            if self.stack.len() < arg_count + 1 {
                                return Err(VMError::StackUnderflow);
                            }
                            let func_index = self.stack.len() - arg_count - 1;
                            let func_val = self.stack.remove(func_index);
                            let args: Vec<Value> = self.stack.drain(self.stack.len()-arg_count..).collect();
                            match func_val {
                                Value::Function(ref func) => {
                                    self.call_stack.pop();
                                    let parent_closure = if let Some(caller) = self.call_stack.last() {
                                        caller.closure.clone()
                                    } else {
                                        HashMap::new()
                                    };
                                    let new_closure = func.closure.clone().unwrap_or(parent_closure);
                                    let new_frame = CallFrame {
                                        code: func.code.clone(),
                                        ip: 0,
                                        constants: func.constants.clone(),
                                        locals: args,
                                        _base: func.base,
                                        closure: new_closure,
                                    };
                                    self.call_stack.push(new_frame);
                                    control_transferred = true;
                                    break;
                                }
                                Value::BuiltinFunction(_, builtin_fn) => {
                                    let ret = builtin_fn(&args)?;
                                    self.call_stack.pop();
                                    self.stack.push(ret);
                                    control_transferred = true;
                                    break;
                                }
                                other => return Err(VMError::TypeError(format!("Attempted to tail call non-function: {}", other))),
                            }
                        }
                        Instruction::Return => {
                            let ret_val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                            self.call_stack.pop();
                            if self.call_stack.is_empty() {
                                self.stack.push(ret_val);
                                return self.stack.pop().ok_or(VMError::StackUnderflow);
                            } else {
                                self.stack.push(ret_val);
                            }
                            control_transferred = true;
                            break;
                        }
                        Instruction::Pop => {
                            self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        }
                    }
                }
                control_transferred
            };
            if transferred {
                continue;
            }
            // If the current frame ran to completion naturally, pop it.
            if let Some(frame) = self.call_stack.last() {
                if frame.ip >= frame.code.len() {
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
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }
}