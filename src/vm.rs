use std::collections::HashMap;
use std::fmt;

use crate::bytecode::Instruction;
use crate::value::Value;

/// Possible runtime errors from the VM.
#[derive(Debug, PartialEq)]
pub enum VMError {
    TypeError(String),
    DivisionByZero,
    StackUnderflow,
    UndefinedVariable(String),
    ReturnValue(Value), // used for "unwinding" from top-level
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

/// A call frame holds local variables and the function’s own bytecode/constants.
struct CallFrame {
    pub code: Vec<Instruction>,
    pub ip: usize,
    pub constants: Vec<Value>,
    pub locals: Vec<Value>,
    pub base: usize,
}

/// The VM carries a stack, a call stack (frames), and a global map of name → Value.
pub struct VM {
    /// Global variables/builtins are stored by name
    pub globals: HashMap<String, Value>,

    /// A stack of active call frames
    call_stack: Vec<CallFrame>,

    /// The operand stack for intermediate values
    stack: Vec<Value>,
}

impl VM {
    /// Create a fresh VM (empty stack, call_stack, and globals).
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            call_stack: Vec::new(),
            stack: Vec::new(),
        }
    }

    /// Insert or update a global value by name (for builtins or top-level variables).
    pub fn define_global(&mut self, name: &str, value: Value) {
        self.globals.insert(name.to_string(), value);
    }

    /// Helper to pop a number from the stack or produce a TypeError.
    fn pop_number(&mut self) -> Result<f64, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Number(n) => Ok(n),
            other => Err(VMError::TypeError(format!("Expected number, got {}", other))),
        }
    }

    /// Helper to pop a bool from the stack or produce a TypeError.
    fn pop_bool(&mut self) -> Result<bool, VMError> {
        match self.stack.pop().ok_or(VMError::StackUnderflow)? {
            Value::Bool(b) => Ok(b),
            other => Err(VMError::TypeError(format!("Expected bool, got {}", other))),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // TOP-LEVEL CODE EXECUTION
    // ─────────────────────────────────────────────────────────────────────────────

    /// Execute code (with its constants) in "top-level" mode (no function frame).
    /// Returns the top of the stack if successful, or an error.
    pub fn run(&mut self, code: &[Instruction], constants: &[Value]) -> Result<Value, VMError> {
        // We'll maintain our own program counter here (ip) for top-level code
        let mut ip = 0;

        // Each iteration, we do a short borrow to fetch an instruction:
        while ip < code.len() {
            // Borrow code/ip in a small scope
            let instr = code[ip].clone();
            ip += 1; // increment IP

            match instr {
                // Loading constants, arithmetic, logic, etc.
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

                // Local variable instructions usually won't appear at top-level;
                // if they do, we'll fail with an error:
                Instruction::LoadLocal(_idx) => {
                    return Err(VMError::TypeError("LoadLocal in top-level".to_string()));
                }
                Instruction::StoreLocal(_idx) => {
                    return Err(VMError::TypeError("StoreLocal in top-level".to_string()));
                }

                // Global variable instructions (by name)
                Instruction::LoadGlobal(name) => {
                    let val = self.globals.get(&name)
                        .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                    self.stack.push(val.clone());
                }
                Instruction::StoreGlobal(name) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.globals.insert(name.clone(), val);
                }

                // Control flow
                Instruction::Jump(target) => {
                    ip = target;
                }
                Instruction::JumpIfFalse(target) => {
                    let cond = self.pop_bool()?;
                    if !cond {
                        ip = target;
                    }
                }

                // Function calls
                Instruction::Call(_func_idx, arg_count) => {
                    // The function object is on top of stack
                    let func_val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    match func_val {
                        Value::Function(func) => {
                            // gather arguments
                            let mut args = Vec::new();
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                            }
                            args.reverse();
                            // push a new call frame
                            let frame = CallFrame {
                                code: func.code.clone(),
                                ip: 0,
                                constants: func.constants.clone(),
                                locals: args,
                                base: func.base,
                            };
                            self.call_stack.push(frame);
                            // run the function in run_frame()
                            let ret = self.run_frame()?;
                            self.stack.push(ret);
                        }
                        Value::BuiltinFunction(_, f) => {
                            // builtin
                            let mut args = Vec::new();
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                            }
                            args.reverse();
                            let ret = f(&args)?;
                            self.stack.push(ret);
                        }
                        other => {
                            return Err(VMError::TypeError(format!("Call of non-function: {}", other)));
                        }
                    }
                }

                // Pop discards top of stack
                Instruction::Pop => {
                    self.stack.pop().ok_or(VMError::StackUnderflow)?;
                }

                // Return from top-level means short-circuit with a “ReturnValue” error
                Instruction::Return => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    return Err(VMError::ReturnValue(val));
                }
            }
        }

        // If we finish the code with no explicit return, pop top-of-stack
        // as the result
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // FUNCTION CALL EXECUTION
    // ─────────────────────────────────────────────────────────────────────────────

    /// Execute instructions in the topmost CallFrame (last in call_stack),
    /// returning the final result of the function.
    fn run_frame(&mut self) -> Result<Value, VMError> {
        loop {
            // Borrow the call frame briefly to fetch the current instruction
            let instr = {
                let frame = self.call_stack.last_mut().ok_or(VMError::StackUnderflow)?;
                if frame.ip >= frame.code.len() {
                    return Err(VMError::StackUnderflow);
                }
                let i = frame.code[frame.ip].clone();
                frame.ip += 1;
                i
            };

            // Now that `frame` is dropped, we can match on `instr` and call &mut self methods.
            match instr {
                Instruction::LoadConst(idx) => {
                    let frame = self.call_stack.last().ok_or(VMError::StackUnderflow)?;
                    let val = frame.constants.get(idx)
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

                // Locals
                Instruction::LoadLocal(idx) => {
                    let frame = self.call_stack.last().ok_or(VMError::StackUnderflow)?;
                    let val = frame.locals.get(idx)
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
                        // expand if needed
                        while frame.locals.len() < idx {
                            frame.locals.push(Value::Number(0.0));
                        }
                        frame.locals.push(val);
                    }
                }

                // Globals
                Instruction::LoadGlobal(name) => {
                    let val = self.globals.get(&name)
                        .ok_or_else(|| VMError::UndefinedVariable(name.clone()))?;
                    self.stack.push(val.clone());
                }
                Instruction::StoreGlobal(name) => {
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.globals.insert(name.clone(), val);
                }

                // Control flow
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

                // Calls
                Instruction::Call(_func_idx, arg_count) => {
                    let func_val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    match func_val {
                        Value::Function(f) => {
                            let mut args = Vec::new();
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                            }
                            args.reverse();

                            let new_frame = CallFrame {
                                code: f.code.clone(),
                                ip: 0,
                                constants: f.constants.clone(),
                                locals: args,
                                base: f.base,
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
                    // pop the return value from stack
                    let val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    // pop this frame
                    self.call_stack.pop();
                    // and return
                    return Ok(val);
                }
            }
        }
    }
}
