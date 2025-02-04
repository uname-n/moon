use crate::ast;
use crate::bytecode::Instruction;
use crate::value::{Function, Value};
use std::sync::Arc;
use num_bigint::BigInt;
use crate::optimizer;

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
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
    fn find_local(&self, name: &str) -> Option<usize> {
        self.variables.iter().position(|n| n == name)
    }
    pub fn compile_program(&mut self, stmts: &[ast::Stmt]) {
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
        self.code.push(Instruction::Return);
        let (opt_code, opt_constants) =
            optimizer::optimize_bytecode(self.code.clone(), self.constants.clone());
        self.code = opt_code;
        self.constants = opt_constants;
    }
    pub fn compile_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Expression(expr) => {
                self.compile_expr(expr);
            }
            ast::Stmt::VariableDeclaration { name, var_type, initializer } => {
                if let Some(expr) = initializer {
                    if let ast::Expr::Number(n) = expr {
                        match var_type {
                            ast::TypeAnnotation::Int => {
                                if n.fract() != 0.0 {
                                    panic!("Type error: variable {} declared as int but initializer is a float literal", name);
                                }
                                let idx = self.add_constant(Value::Integer(BigInt::from(*n as i64)));
                                self.code.push(Instruction::LoadConst(idx));
                            }
                            ast::TypeAnnotation::Float => {
                                let idx = self.add_constant(Value::Float(*n));
                                self.code.push(Instruction::LoadConst(idx));
                            }
                            _ => {
                                self.compile_expr(expr);
                            }
                        }
                    } else {
                        self.compile_expr(expr);
                    }
                } else {
                    let idx = match var_type {
                        ast::TypeAnnotation::Int => self.add_constant(Value::Integer(BigInt::from(0))),
                        ast::TypeAnnotation::Float => self.add_constant(Value::Float(0.0)),
                        _ => self.add_constant(Value::Integer(BigInt::from(0))),
                    };
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
            ast::Stmt::Assignment { name, expr } => {
                self.compile_expr(expr);
                if let Some(local_idx) = self.find_local(name) {
                    self.code.push(Instruction::StoreLocal(local_idx));
                } else {
                    self.code.push(Instruction::StoreGlobal(name.clone()));
                }
            }
            ast::Stmt::If { condition, then_branch, else_branch } => {
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
            ast::Stmt::While { condition, body } => {
                let loop_start = self.code.len();
                self.compile_expr(condition);
                let jump_idx = self.code.len();
                self.code.push(Instruction::JumpIfFalse(0));
                for stmt in body {
                    self.compile_stmt(stmt);
                }
                self.code.push(Instruction::Jump(loop_start));
                let loop_end = self.code.len();
                self.code[jump_idx] = Instruction::JumpIfFalse(loop_end);
            }
            ast::Stmt::FunctionDeclaration { name, params, return_type: _, body } => {
                let mut func_compiler = Compiler::new();
                func_compiler.is_top_level = false;
                func_compiler.variables = params.iter().map(|(p, _)| p.clone()).collect();
                for stmt in body {
                    func_compiler.compile_stmt(stmt);
                }
                let zero_idx = func_compiler.add_constant(Value::Integer(BigInt::from(0)));
                func_compiler.code.push(Instruction::LoadConst(zero_idx));
                func_compiler.code.push(Instruction::Return);
                let function_val = Value::Function(Function {
                    name: name.clone(),
                    params: params.iter().map(|(p, _)| p.clone()).collect(),
                    code: Arc::new(func_compiler.code),
                    constants: Arc::new(func_compiler.constants),
                    base: 0,
                    closure: if self.is_top_level {
                        None
                    } else {
                        Some(std::collections::HashMap::new())
                    },
                });
                let const_idx = self.add_constant(function_val);
                self.code.push(Instruction::LoadConst(const_idx));
                self.code.push(Instruction::StoreGlobal(name.clone()));
            }
            ast::Stmt::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    match expr {
                        ast::Expr::Call { callee, arguments } => {
                            self.compile_expr(callee);
                            for arg in arguments.iter() {
                                self.compile_expr(arg);
                            }
                            self.code.push(Instruction::TailCall(0, arguments.len()));
                        }
                        _ => {
                            self.compile_expr(expr);
                            self.code.push(Instruction::Return);
                        }
                    }
                } else {
                    let idx = self.add_constant(Value::Integer(BigInt::from(0)));
                    self.code.push(Instruction::LoadConst(idx));
                    self.code.push(Instruction::Return);
                }
            }
            ast::Stmt::Print(args) => {
                self.code.push(Instruction::LoadGlobal("print".to_string()));
                for arg in args.iter() {
                    self.compile_expr(arg);
                }
                self.code.push(Instruction::Call(0, args.len()));
            }
        }
    }
    pub fn compile_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Number(n) => {
                let val = if n.fract() == 0.0 {
                    Value::Integer(BigInt::from(*n as i64))
                } else {
                    Value::Float(*n)
                };
                let idx = self.add_constant(val);
                self.code.push(Instruction::LoadConst(idx));
            }
            ast::Expr::Bool(b) => {
                let idx = self.add_constant(Value::Bool(*b));
                self.code.push(Instruction::LoadConst(idx));
            }
            ast::Expr::Str(s) => {
                let idx = self.add_constant(Value::Str(s.clone()));
                self.code.push(Instruction::LoadConst(idx));
            }
            ast::Expr::Identifier(name) => {
                if let Some(local_idx) = self.find_local(name) {
                    self.code.push(Instruction::LoadLocal(local_idx));
                } else {
                    self.code.push(Instruction::LoadGlobal(name.clone()));
                }
            }
            ast::Expr::Unary { op, expr } => {
                self.compile_expr(expr);
                match op {
                    ast::UnaryOp::Negate => self.code.push(Instruction::Negate),
                    ast::UnaryOp::Not => self.code.push(Instruction::Not),
                }
            }
            ast::Expr::Binary { left, op, right } => match op {
                ast::BinaryOp::And => {
                    self.compile_expr(left);
                    let jump_if_false_idx = self.code.len();
                    self.code.push(Instruction::JumpIfFalse(0));
                    self.compile_expr(right);
                    let jump_to_end_idx = self.code.len();
                    self.code.push(Instruction::Jump(0));
                    let false_label = self.code.len();
                    let false_const = self.add_constant(Value::Bool(false));
                    self.code.push(Instruction::LoadConst(false_const));
                    let end_label = self.code.len();
                    self.code[jump_if_false_idx] = Instruction::JumpIfFalse(false_label);
                    self.code[jump_to_end_idx] = Instruction::Jump(end_label);
                }
                ast::BinaryOp::Or => {
                    self.compile_expr(left);
                    let jump_if_true_idx = self.code.len();
                    self.code.push(Instruction::JumpIfTrue(0));
                    self.compile_expr(right);
                    let jump_to_end_idx = self.code.len();
                    self.code.push(Instruction::Jump(0));
                    let true_label = self.code.len();
                    let true_const = self.add_constant(Value::Bool(true));
                    self.code.push(Instruction::LoadConst(true_const));
                    let end_label = self.code.len();
                    self.code[jump_if_true_idx] = Instruction::JumpIfTrue(true_label);
                    self.code[jump_to_end_idx] = Instruction::Jump(end_label);
                }
                ast::BinaryOp::Add => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Add);
                }
                ast::BinaryOp::Subtract => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Sub);
                }
                ast::BinaryOp::Multiply => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Mul);
                }
                ast::BinaryOp::Divide => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Div);
                }
                ast::BinaryOp::Equal => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Equal);
                }
                ast::BinaryOp::NotEqual => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::NotEqual);
                }
                ast::BinaryOp::Less => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Less);
                }
                ast::BinaryOp::Greater => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Greater);
                }
                ast::BinaryOp::LessEqual => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::LessEqual);
                }
                ast::BinaryOp::GreaterEqual => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::GreaterEqual);
                }
            },
            ast::Expr::Call { callee, arguments } => {
                self.compile_expr(callee);
                for arg in arguments.iter() {
                    self.compile_expr(arg);
                }
                self.code.push(Instruction::Call(0, arguments.len()));
            }
        }
    }
}