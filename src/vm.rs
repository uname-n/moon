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
        'vm_loop: loop {
            let frame_index = self.call_stack.len() - 1;
            {
                let frame = &mut self.call_stack[frame_index];
                let code_arc = frame.code.clone();
                let code_slice = code_arc.as_ref();
                let code_len = code_slice.len();
                let mut ip = frame.ip;
                let constants_arc = frame.constants.clone();
                let mut locals = std::mem::take(&mut frame.locals);
                let mut closure = std::mem::take(&mut frame.closure);
                {
                    let stack = &mut self.stack;
                    while ip < code_len {
                        let instr = unsafe { code_slice.get_unchecked(ip) }.clone();
                        ip += 1;
                        match instr {
                            Instruction::LoadConst(idx) => {
                                let val = constants_arc.get(idx).ok_or_else(|| VMError::TypeError(format!("No constant at index {}", idx)))?.clone();
                                stack.push(val);
                            }
                            Instruction::Add => {
                                let b = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                let a = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Number(a + b));
                            }
                            Instruction::Sub => {
                                let b = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                let a = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Number(a - b));
                            }
                            Instruction::Mul => {
                                let b = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                let a = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Number(a * b));
                            }
                            Instruction::Div => {
                                let b = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                if b == 0.0 {
                                    return Err(VMError::DivisionByZero);
                                }
                                let a = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Number(a / b));
                            }
                            Instruction::Negate => {
                                let n = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Number(-n));
                            }
                            Instruction::Not => {
                                let b = match stack.pop() {
                                    Some(Value::Bool(b)) => b,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected bool, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Bool(!b));
                            }
                            Instruction::Equal => {
                                let b = stack.pop().ok_or(VMError::StackUnderflow)?;
                                let a = stack.pop().ok_or(VMError::StackUnderflow)?;
                                stack.push(Value::Bool(a == b));
                            }
                            Instruction::NotEqual => {
                                let b = stack.pop().ok_or(VMError::StackUnderflow)?;
                                let a = stack.pop().ok_or(VMError::StackUnderflow)?;
                                stack.push(Value::Bool(a != b));
                            }
                            Instruction::Less => {
                                let b = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                let a = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Bool(a < b));
                            }
                            Instruction::Greater => {
                                let b = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                let a = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Bool(a > b));
                            }
                            Instruction::LessEqual => {
                                let b = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                let a = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Bool(a <= b));
                            }
                            Instruction::GreaterEqual => {
                                let b = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                let a = match stack.pop() {
                                    Some(Value::Number(n)) => n,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected number, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                stack.push(Value::Bool(a >= b));
                            }
                            Instruction::LoadLocal(idx) => {
                                let val = locals.get(idx).ok_or_else(|| VMError::UndefinedVariable(format!("local#{}", idx)))?.clone();
                                stack.push(val);
                            }
                            Instruction::StoreLocal(idx) => {
                                let val = stack.pop().ok_or(VMError::StackUnderflow)?;
                                if idx < locals.len() {
                                    locals[idx] = val;
                                } else {
                                    while locals.len() < idx {
                                        locals.push(Value::Number(0.0));
                                    }
                                    locals.push(val);
                                }
                            }
                            Instruction::LoadGlobal(ref name) => {
                                let val = self.globals.get(name).ok_or_else(|| VMError::UndefinedVariable(name.clone()))?.clone();
                                stack.push(val);
                            }
                            Instruction::StoreGlobal(ref name) => {
                                let val = stack.pop().ok_or(VMError::StackUnderflow)?;
                                self.globals.insert(name.clone(), val);
                            }
                            Instruction::LoadClosure(ref name) => {
                                let val = closure.get(name).cloned().ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                                stack.push(val);
                            }
                            Instruction::StoreClosure(ref name) => {
                                let val = stack.pop().ok_or(VMError::StackUnderflow)?;
                                closure.insert(name.clone(), val);
                            }
                            Instruction::Jump(target) => {
                                ip = target;
                            }
                            Instruction::JumpIfFalse(target) => {
                                let cond = match stack.pop() {
                                    Some(Value::Bool(b)) => b,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected bool, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                if !cond {
                                    ip = target;
                                }
                            }
                            Instruction::JumpIfTrue(target) => {
                                let cond = match stack.pop() {
                                    Some(Value::Bool(b)) => b,
                                    Some(other) => return Err(VMError::TypeError(format!("Expected bool, got {}", other))),
                                    None => return Err(VMError::StackUnderflow),
                                };
                                if cond {
                                    ip = target;
                                }
                            }
                            Instruction::Call(_, arg_count) => {
                                frame.ip = ip;
                                frame.locals = locals.clone();
                                frame.closure = closure.clone();
                                if stack.len() < arg_count + 1 {
                                    return Err(VMError::StackUnderflow);
                                }
                                let func_index = stack.len() - arg_count - 1;
                                let func_val = stack.remove(func_index);
                                let start = stack.len() - arg_count;
                                let args: Vec<Value> = stack.drain(start..).collect();
                                match func_val {
                                    Value::Function(ref func) => {
                                        let new_closure = if let Some(ref c) = func.closure { c.clone() } else { closure.clone() };
                                        self.call_stack.push(CallFrame {
                                            code: func.code.clone(),
                                            ip: 0,
                                            constants: func.constants.clone(),
                                            locals: args,
                                            _base: func.base,
                                            closure: new_closure,
                                        });
                                        continue 'vm_loop;
                                    }
                                    Value::BuiltinFunction(_, builtin_fn) => {
                                        let ret = builtin_fn(&args)?;
                                        stack.push(ret);
                                    }
                                    other => return Err(VMError::TypeError(format!("Attempted to call non-function: {}", other))),
                                }
                            }
                            Instruction::TailCall(_, arg_count) => {
                                frame.ip = ip;
                                frame.locals = locals.clone();
                                frame.closure = closure.clone();
                                if stack.len() < arg_count + 1 {
                                    return Err(VMError::StackUnderflow);
                                }
                                let func_index = stack.len() - arg_count - 1;
                                let func_val = stack.remove(func_index);
                                let start = stack.len() - arg_count;
                                let args: Vec<Value> = stack.drain(start..).collect();
                                match func_val {
                                    Value::Function(ref func) => {
                                        self.call_stack.pop();
                                        let parent = if let Some(prev) = self.call_stack.last() { prev.closure.clone() } else { HashMap::new() };
                                        let new_closure = if let Some(ref c) = func.closure { c.clone() } else { parent };
                                        self.call_stack.push(CallFrame {
                                            code: func.code.clone(),
                                            ip: 0,
                                            constants: func.constants.clone(),
                                            locals: args,
                                            _base: func.base,
                                            closure: new_closure,
                                        });
                                        continue 'vm_loop;
                                    }
                                    Value::BuiltinFunction(_, builtin_fn) => {
                                        let ret = builtin_fn(&args)?;
                                        self.call_stack.pop();
                                        if self.call_stack.is_empty() {
                                            return Ok(ret);
                                        } else {
                                            stack.push(ret);
                                        }
                                        continue 'vm_loop;
                                    }
                                    other => return Err(VMError::TypeError(format!("Attempted to tail call non-function: {}", other))),
                                }
                            }
                            Instruction::Pop => {
                                stack.pop().ok_or(VMError::StackUnderflow)?;
                            }
                            Instruction::Return => {
                                let ret_val = stack.pop().ok_or(VMError::StackUnderflow)?;
                                self.call_stack.pop();
                                if self.call_stack.is_empty() {
                                    return Ok(ret_val);
                                } else {
                                    stack.push(ret_val);
                                    continue 'vm_loop;
                                }
                            }
                        }
                    }
                }
                frame.ip = ip;
                frame.locals = locals.clone();
                frame.closure = closure.clone();
            }
            if self.call_stack.is_empty() {
                break 'vm_loop;
            }
            {
                let stack = &mut self.stack;
                let ret_val = stack.pop().ok_or(VMError::StackUnderflow)?;
                stack.push(ret_val);
            }
        }
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }
}