use crate::ast::*;
use crate::bytecode::Instruction;
use crate::value::{Function, Value};

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
            Stmt::VariableDeclaration {
                name,
                initializer,
                var_type: _,
            } => {
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
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
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
            Stmt::While { condition, body } => {
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
            Stmt::FunctionDeclaration {
                name,
                params,
                return_type: _,
                body,
            } => {
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
                for arg in args.iter() {
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
            Expr::Str(s) => {
                let idx = self.add_constant(Value::Str(s.clone()));
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
            Expr::Binary { left, op, right } => match op {
                BinaryOp::And => {
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
                BinaryOp::Or => {
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
                BinaryOp::Add => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Add);
                }
                BinaryOp::Subtract => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Sub);
                }
                BinaryOp::Multiply => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Mul);
                }
                BinaryOp::Divide => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Div);
                }
                BinaryOp::Equal => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Equal);
                }
                BinaryOp::NotEqual => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::NotEqual);
                }
                BinaryOp::Less => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Less);
                }
                BinaryOp::Greater => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::Greater);
                }
                BinaryOp::LessEqual => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::LessEqual);
                }
                BinaryOp::GreaterEqual => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.push(Instruction::GreaterEqual);
                }
            },
            Expr::Call { callee, arguments } => {
                for arg in arguments.iter() {
                    self.compile_expr(arg);
                }

                self.compile_expr(callee);

                self.code.push(Instruction::Call(0, arguments.len()));
            }
        }
    }
}
