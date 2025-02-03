use crate::ast::*;
use crate::bytecode::Instruction;
use crate::value::{Value, Function};

pub struct Compiler {
    pub code: Vec<Instruction>,
    pub constants: Vec<Value>,
    pub variables: Vec<String>,
    pub is_top_level: bool,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            variables: Vec::new(),
            is_top_level: true,
        }
    }

    pub fn compile_program(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
    }

    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn find_local(&self, name: &str) -> Option<usize> {
        self.variables.iter().position(|n| n == name)
    }

    pub fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr) => {
                self.compile_expr(expr);
            }
            Stmt::VariableDeclaration { name, initializer, .. } => {
                if let Some(expr) = initializer {
                    self.compile_expr(expr);
                } else {
                    let idx = self.add_constant(Value::Number(0.0));
                    self.code.push(Instruction::LoadConst(idx));
                }
                if self.is_top_level {
                    self.code.push(Instruction::StoreGlobal(name.clone()));
                } else {
                    let local_idx = self.variables.len();
                    self.variables.push(name.clone());
                    self.code.push(Instruction::StoreLocal(local_idx));
                }
            }
            Stmt::Assignment { name, expr } => {
                self.compile_expr(expr);
                if let Some(local_idx) = self.find_local(name) {
                    self.code.push(Instruction::StoreLocal(local_idx));
                } else {
                    self.code.push(Instruction::StoreGlobal(name.clone()));
                }
            }
            Stmt::If { condition, then_branch, else_branch } => {
                self.compile_expr(condition);
                let jump_if_false_idx = self.code.len();
                self.code.push(Instruction::JumpIfFalse(0));
                for s in then_branch {
                    self.compile_stmt(s);
                }
                let jump_idx = self.code.len();
                self.code.push(Instruction::Jump(0));
                let else_start = self.code.len();
                self.code[jump_if_false_idx] = Instruction::JumpIfFalse(else_start);
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        self.compile_stmt(s);
                    }
                }
                let after_else = self.code.len();
                self.code[jump_idx] = Instruction::Jump(after_else);
            }
            Stmt::FunctionDeclaration { name, params, body, .. } => {
                let mut func_compiler = Compiler::new();
                func_compiler.is_top_level = false;
                func_compiler.variables = params.iter().map(|(p, _)| p.clone()).collect();
                for stmt in body {
                    func_compiler.compile_stmt(stmt);
                }
                let zero_idx = func_compiler.add_constant(Value::Number(0.0));
                func_compiler.code.push(Instruction::LoadConst(zero_idx));
                func_compiler.code.push(Instruction::Return);
                let function_val = Value::Function(Function {
                    name: name.clone(),
                    params: params.iter().map(|(p, _)| p.clone()).collect(),
                    code: func_compiler.code,
                    constants: func_compiler.constants,
                    base: 0,
                });
                let const_idx = self.add_constant(function_val);
                self.code.push(Instruction::LoadConst(const_idx));
                self.code.push(Instruction::StoreGlobal(name.clone()));
            }
            Stmt::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.compile_expr(expr);
                } else {
                    let idx = self.add_constant(Value::Number(0.0));
                    self.code.push(Instruction::LoadConst(idx));
                }
                self.code.push(Instruction::Return);
            }
            Stmt::Print(args) => {
                for arg in args.iter().rev() {
                    self.compile_expr(arg);
                }
                self.code.push(Instruction::LoadGlobal("print".to_string()));
                self.code.push(Instruction::Call(0, args.len()));
            }
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
            Expr::Identifier(name) => {
                if let Some(local_idx) = self.find_local(name) {
                    self.code.push(Instruction::LoadLocal(local_idx));
                } else {
                    self.code.push(Instruction::LoadGlobal(name.clone()));
                }
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
                    BinaryOp::Equal => self.code.push(Instruction::Equal),
                    _ => unimplemented!("Operator not implemented in compiler"),
                }
            }
            Expr::Call { callee, arguments } => {
                for arg in arguments.iter().rev() {
                    self.compile_expr(arg);
                }
                self.compile_expr(callee);
                self.code.push(Instruction::Call(0, arguments.len()));
            }
        }
    }
}