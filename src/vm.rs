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
    #[inline]
    fn pop_number_from(stack: &mut Vec<Value>) -> Result<f64, VMError> {
        match stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Number(n) => Ok(n),
            other => Err(VMError::TypeError(format!("Expected number, got {}", other))),
        }
    }
    #[inline]
    fn pop_bool_from(stack: &mut Vec<Value>) -> Result<bool, VMError> {
        match stack.pop().ok_or(VMError::StackUnderflow)? {
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
            let frame = self.call_stack.last_mut().unwrap();
            let instructions = &frame.code;
            let mut ip = frame.ip;
            while ip < instructions.len() {
                let instr = instructions[ip].clone();
                ip += 1;
                match instr {
                    Instruction::LoadConst(idx) => {
                        let val = frame.constants.get(idx).ok_or_else(|| VMError::TypeError(format!("No constant at index {}", idx)))?.clone();
                        self.stack.push(val);
                    }
                    Instruction::Add => {
                        let b = VM::pop_number_from(&mut self.stack)?;
                        let a = VM::pop_number_from(&mut self.stack)?;
                        self.stack.push(Value::Number(a + b));
                    }
                    Instruction::Sub => {
                        let b = VM::pop_number_from(&mut self.stack)?;
                        let a = VM::pop_number_from(&mut self.stack)?;
                        self.stack.push(Value::Number(a - b));
                    }
                    Instruction::Mul => {
                        let b = VM::pop_number_from(&mut self.stack)?;
                        let a = VM::pop_number_from(&mut self.stack)?;
                        self.stack.push(Value::Number(a * b));
                    }
                    Instruction::Div => {
                        let b = VM::pop_number_from(&mut self.stack)?;
                        if b == 0.0 {
                            return Err(VMError::DivisionByZero);
                        }
                        let a = VM::pop_number_from(&mut self.stack)?;
                        self.stack.push(Value::Number(a / b));
                    }
                    Instruction::Negate => {
                        let n = VM::pop_number_from(&mut self.stack)?;
                        self.stack.push(Value::Number(-n));
                    }
                    Instruction::Not => {
                        let b = VM::pop_bool_from(&mut self.stack)?;
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
                        let b = VM::pop_number_from(&mut self.stack)?;
                        let a = VM::pop_number_from(&mut self.stack)?;
                        self.stack.push(Value::Bool(a < b));
                    }
                    Instruction::Greater => {
                        let b = VM::pop_number_from(&mut self.stack)?;
                        let a = VM::pop_number_from(&mut self.stack)?;
                        self.stack.push(Value::Bool(a > b));
                    }
                    Instruction::LessEqual => {
                        let b = VM::pop_number_from(&mut self.stack)?;
                        let a = VM::pop_number_from(&mut self.stack)?;
                        self.stack.push(Value::Bool(a <= b));
                    }
                    Instruction::GreaterEqual => {
                        let b = VM::pop_number_from(&mut self.stack)?;
                        let a = VM::pop_number_from(&mut self.stack)?;
                        self.stack.push(Value::Bool(a >= b));
                    }
                    Instruction::LoadLocal(idx) => {
                        let val = frame.locals.get(idx).ok_or_else(|| VMError::UndefinedVariable(format!("local#{}", idx)))?.clone();
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
                        let val = self.globals.get(name).ok_or_else(|| VMError::UndefinedVariable(name.clone()))?.clone();
                        self.stack.push(val);
                    }
                    Instruction::StoreGlobal(ref name) => {
                        let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        self.globals.insert(name.clone(), val);
                    }
                    Instruction::LoadClosure(ref name) => {
                        let val = frame.closure.get(name).cloned().ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                        self.stack.push(val);
                    }
                    Instruction::StoreClosure(ref name) => {
                        let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        frame.closure.insert(name.clone(), val);
                    }
                    Instruction::Jump(target) => {
                        ip = target;
                    }
                    Instruction::JumpIfFalse(target) => {
                        let cond = VM::pop_bool_from(&mut self.stack)?;
                        if !cond {
                            ip = target;
                        }
                    }
                    Instruction::JumpIfTrue(target) => {
                        let cond = VM::pop_bool_from(&mut self.stack)?;
                        if cond {
                            ip = target;
                        }
                    }
                    Instruction::Call(_, arg_count) => {
                        if self.stack.len() < arg_count + 1 {
                            return Err(VMError::StackUnderflow);
                        }
                        frame.ip = ip;
                        let func_index = self.stack.len() - arg_count - 1;
                        let func_val = self.stack.remove(func_index);
                        let args = self.stack.split_off(self.stack.len() - arg_count);
                        match func_val {
                            Value::Function(ref func) => {
                                let parent = frame.closure.clone();
                                let closure = if let Some(ref c) = func.closure { c.clone() } else { parent };
                                self.call_stack.push(CallFrame {
                                    code: func.code.clone(),
                                    ip: 0,
                                    constants: func.constants.clone(),
                                    locals: args,
                                    _base: func.base,
                                    closure,
                                });
                                break;
                            }
                            Value::BuiltinFunction(_, builtin_fn) => {
                                let ret = builtin_fn(&args)?;
                                self.stack.push(ret);
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
                        let args = self.stack.split_off(self.stack.len() - arg_count);
                        match func_val {
                            Value::Function(ref func) => {
                                let parent = frame.closure.clone();
                                let closure = if let Some(ref c) = func.closure { c.clone() } else { parent };
                                self.call_stack.pop();
                                self.call_stack.push(CallFrame {
                                    code: func.code.clone(),
                                    ip: 0,
                                    constants: func.constants.clone(),
                                    locals: args,
                                    _base: func.base,
                                    closure,
                                });
                                break;
                            }
                            Value::BuiltinFunction(_, builtin_fn) => {
                                let ret = builtin_fn(&args)?;
                                self.stack.push(ret.clone());
                                if self.call_stack.len() == 1 {
                                    return Ok(ret);
                                } else {
                                    self.stack.push(ret);
                                }
                                break;
                            }
                            other => return Err(VMError::TypeError(format!("Attempted to tail call non-function: {}", other))),
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
                            break;
                        }
                    }
                }
            }
            if let Some(frame) = self.call_stack.last_mut() {
                frame.ip = ip;
                if ip >= frame.code.len() {
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
    }
}