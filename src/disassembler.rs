use crate::bytecode::Instruction;
use crate::value::Value;

pub fn disassemble(code: &[Instruction], constants: &[Value]) -> String {
    let mut output = String::new();
    for (i, instr) in code.iter().enumerate() {
        output.push_str(&format!("{:04}    {}\n", i, format_instruction(instr, constants)));
    }
    output
}

fn format_instruction(instr: &Instruction, constants: &[Value]) -> String {
    match instr {
        Instruction::LoadConst(idx) => format!("LoadConst {}", constants.get(*idx).map_or("?".to_string(), |v| v.to_string())),
        Instruction::Add => "Add".to_string(),
        Instruction::Sub => "Sub".to_string(),
        Instruction::Mul => "Mul".to_string(),
        Instruction::Div => "Div".to_string(),
        Instruction::Negate => "Negate".to_string(),
        Instruction::Not => "Not".to_string(),
        Instruction::Equal => "Equal".to_string(),
        Instruction::NotEqual => "NotEqual".to_string(),
        Instruction::Less => "Less".to_string(),
        Instruction::Greater => "Greater".to_string(),
        Instruction::LessEqual => "LessEqual".to_string(),
        Instruction::GreaterEqual => "GreaterEqual".to_string(),
        Instruction::LoadLocal(idx) => format!("LoadLocal {}", idx),
        Instruction::StoreLocal(idx) => format!("StoreLocal {}", idx),
        Instruction::LoadGlobal(name) => format!("LoadGlobal {}", name),
        Instruction::StoreGlobal(name) => format!("StoreGlobal {}", name),
        Instruction::LoadClosure(name) => format!("LoadClosure {}", name),
        Instruction::StoreClosure(name) => format!("StoreClosure {}", name),
        Instruction::Jump(target) => format!("Jump {}", target),
        Instruction::JumpIfFalse(target) => format!("JumpIfFalse {}", target),
        Instruction::JumpIfTrue(target) => format!("JumpIfTrue {}", target),
        Instruction::Call(_, arg_count) => format!("Call {}", arg_count),
        Instruction::TailCall(_, arg_count) => format!("TailCall {}", arg_count),
        Instruction::Return => "Return".to_string(),
        Instruction::Pop => "Pop".to_string(),
    }
}