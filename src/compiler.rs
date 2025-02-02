use crate::ast::{Expr, UnaryOp, BinaryOp};
use crate::bytecode::Instruction;
use crate::value::Value;

pub struct Compiler {
    pub code: Vec<Instruction>,
    pub constants: Vec<Value>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Number(n) => {
                let idx = self.add_constant(Value::Number(*n));
                self.code.push(Instruction::LoadConst(idx));
            }
            Expr::Bool(b) => {
                let idx = self.add_constant(Value::Bool(*b));
                self.code.push(Instruction::LoadConst(idx));
            }
            Expr::Unary { op, expr } => {
                self.compile_expr(expr);
                match op {
                    UnaryOp::Negate => self.code.push(Instruction::Negate),
                    UnaryOp::Not => self.code.push(Instruction::Not),
                }
            }
            Expr::Binary { left, op, right } => {
                self.compile_expr(left);
                self.compile_expr(right);
                match op {
                    BinaryOp::Add => self.code.push(Instruction::Add),
                    BinaryOp::Subtract => self.code.push(Instruction::Sub),
                    BinaryOp::Multiply => self.code.push(Instruction::Mul),
                    BinaryOp::Divide => self.code.push(Instruction::Div),
                    _ => unimplemented!("Operator not implemented in compiler"),
                }
            }
        }
    }

    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}
