use crate::bytecode::Instruction;
use crate::value::Value;
use num_traits::{ToPrimitive, Zero};

pub fn optimize_bytecode(code: Vec<Instruction>, constants: Vec<Value>) -> (Vec<Instruction>, Vec<Value>) {
    let mut new_code = Vec::new();
    let mut i = 0;
    let mut new_constants = constants;
    while i < code.len() {
        match &code[i] {
            Instruction::LoadConst(idx1) => {
                if i + 2 < code.len() {
                    match (&code[i + 1], &code[i + 2]) {
                        (Instruction::LoadConst(idx2), op) => {
                            if let Some(folded) = fold_binary(op, &new_constants[*idx1], &new_constants[*idx2]) {
                                let const_idx = new_constants.len();
                                new_constants.push(folded);
                                new_code.push(Instruction::LoadConst(const_idx));
                                i += 3;
                                continue;
                            }
                        }
                        _ => {}
                    }
                }
                new_code.push(code[i].clone());
                i += 1;
            }
            Instruction::Negate => {
                if !new_code.is_empty() {
                    if let Instruction::LoadConst(idx) = new_code.pop().unwrap() {
                        if let Some(folded) = fold_unary(&Instruction::Negate, &new_constants[idx]) {
                            let const_idx = new_constants.len();
                            new_constants.push(folded);
                            new_code.push(Instruction::LoadConst(const_idx));
                            i += 1;
                            continue;
                        } else {
                            new_code.push(Instruction::LoadConst(idx));
                        }
                    } else {
                        new_code.push(Instruction::Negate);
                    }
                } else {
                    new_code.push(Instruction::Negate);
                }
                i += 1;
            }
            Instruction::Not => {
                if !new_code.is_empty() {
                    if let Instruction::LoadConst(idx) = new_code.pop().unwrap() {
                        if let Some(folded) = fold_unary(&Instruction::Not, &new_constants[idx]) {
                            let const_idx = new_constants.len();
                            new_constants.push(folded);
                            new_code.push(Instruction::LoadConst(const_idx));
                            i += 1;
                            continue;
                        } else {
                            new_code.push(Instruction::LoadConst(idx));
                        }
                    } else {
                        new_code.push(Instruction::Not);
                    }
                } else {
                    new_code.push(Instruction::Not);
                }
                i += 1;
            }
            Instruction::Jump(target) => {
                if *target == new_code.len() + 1 {
                    i += 1;
                    continue;
                }
                new_code.push(code[i].clone());
                i += 1;
            }
            _ => {
                new_code.push(code[i].clone());
                i += 1;
            }
        }
    }
    let (final_code, final_constants) = inline_functions(new_code, new_constants);
    let optimized_code = dead_code_elimination(final_code);
    (optimized_code, final_constants)
}

fn dead_code_elimination(code: Vec<Instruction>) -> Vec<Instruction> {
    code
}

fn fold_binary(op: &Instruction, v1: &Value, v2: &Value) -> Option<Value> {
    match op {
        Instruction::Add => match (v1, v2) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a + b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a + b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(a.to_f64().unwrap() + b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a + b.to_f64().unwrap())),
            _ => None,
        },
        Instruction::Sub => match (v1, v2) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a - b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a - b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(a.to_f64().unwrap() - b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a - b.to_f64().unwrap())),
            _ => None,
        },
        Instruction::Mul => match (v1, v2) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a * b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a * b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(a.to_f64().unwrap() * b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a * b.to_f64().unwrap())),
            _ => None,
        },
        Instruction::Div => match (v1, v2) {
            (Value::Integer(a), Value::Integer(b)) => {
                if b.is_zero() { None } else { Some(Value::Integer(a / b)) }
            }
            (Value::Float(a), Value::Float(b)) => {
                if *b == 0.0 { None } else { Some(Value::Float(a / b)) }
            }
            (Value::Integer(a), Value::Float(b)) => {
                if *b == 0.0 { None } else { Some(Value::Float(a.to_f64().unwrap() / b)) }
            }
            (Value::Float(a), Value::Integer(b)) => {
                let b_float = b.to_f64().unwrap();
                if b_float == 0.0 { None } else { Some(Value::Float(a / b_float)) }
            }
            _ => None,
        },
        Instruction::Equal => Some(Value::Bool(v1 == v2)),
        Instruction::NotEqual => Some(Value::Bool(v1 != v2)),
        Instruction::Less => match (v1, v2) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Bool(a < b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Bool(a < b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Bool(a.to_f64().unwrap() < *b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Bool(*a < b.to_f64().unwrap())),
            _ => None,
        },
        Instruction::Greater => match (v1, v2) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Bool(a > b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Bool(a > b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Bool(a.to_f64().unwrap() > *b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Bool(*a > b.to_f64().unwrap())),
            _ => None,
        },
        Instruction::LessEqual => match (v1, v2) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Bool(a <= b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Bool(a <= b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Bool(a.to_f64().unwrap() <= *b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Bool(*a <= b.to_f64().unwrap())),
            _ => None,
        },
        Instruction::GreaterEqual => match (v1, v2) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Bool(a >= b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Bool(a >= b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Bool(a.to_f64().unwrap() >= *b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Bool(*a >= b.to_f64().unwrap())),
            _ => None,
        },
        _ => None,
    }
}

fn fold_unary(op: &Instruction, v: &Value) -> Option<Value> {
    match op {
        Instruction::Negate => match v {
            Value::Integer(a) => Some(Value::Integer(-a)),
            Value::Float(a) => Some(Value::Float(-a)),
            _ => None,
        },
        Instruction::Not => {
            if let Value::Bool(b) = v {
                Some(Value::Bool(!b))
            } else {
                None
            }
        },
        _ => None,
    }
}

fn inline_functions(code: Vec<Instruction>, constants: Vec<Value>) -> (Vec<Instruction>, Vec<Value>) {
    let mut new_code = Vec::new();
    let new_constants = constants;
    let mut i = 0;
    while i < code.len() {
        if i + 1 < code.len() {
            if let Instruction::LoadConst(func_idx) = &code[i] {
                if let Instruction::Call(_, arg_count) = &code[i + 1] {
                    if let Some(Value::Function(func)) = new_constants.get(*func_idx) {
                        if *arg_count == 0 && func.params.is_empty() && func.code.len() == 2 {
                            if let Instruction::LoadConst(d) = func.code[0] {
                                if let Instruction::Return = func.code[1] {
                                    new_code.push(Instruction::LoadConst(d));
                                    i += 2;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
        }
        new_code.push(code[i].clone());
        i += 1;
    }
    (new_code, new_constants)
}