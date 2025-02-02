use crate::ast::*;
use crate::bytecode::Instruction;
use crate::value::{Value, Function};

pub struct Compiler {
    pub code: Vec<Instruction>,
    pub constants: Vec<Value>,
    pub variables: Vec<String>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            variables: Vec::new(),
        }
    }
    
    pub fn compile_program(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
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
                self.variables.push(name.clone());
                let var_index = self.variables.len() - 1;
                self.code.push(Instruction::StoreVar(var_index));
            }
            Stmt::Assignment { name, expr } => {
                self.compile_expr(expr);
                if let Some(var_index) = self.variables.iter().position(|n| n == name) {
                    self.code.push(Instruction::StoreVar(var_index));
                } else {
                    panic!("Undefined variable {}", name);
                }
            }
            Stmt::If { condition, then_branch, else_branch } => {
                self.compile_expr(condition);
                let jump_if_false_index = self.code.len();
                self.code.push(Instruction::JumpIfFalse(0));
                for s in then_branch {
                    self.compile_stmt(s);
                }
                let jump_index = self.code.len();
                self.code.push(Instruction::Jump(0));
                let else_start = self.code.len();
                self.code[jump_if_false_index] = Instruction::JumpIfFalse(else_start);
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        self.compile_stmt(s);
                    }
                }
                let after_else = self.code.len();
                self.code[jump_index] = Instruction::Jump(after_else);
            }
            Stmt::FunctionDeclaration { name, params, body, .. } => {
                let global_index = self.variables.len();
                self.variables.push(name.clone());
                let mut func_compiler = Compiler::new();
                func_compiler.variables = self.variables.clone();
                let base = func_compiler.variables.len();
                for (param, _) in params {
                    func_compiler.variables.push(param.clone());
                }
                for stmt in body {
                    func_compiler.compile_stmt(stmt);
                }
                let const_index = func_compiler.add_constant(Value::Number(0.0));
                func_compiler.code.push(Instruction::LoadConst(const_index));
                func_compiler.code.push(Instruction::Return);
                let param_names = params.iter().map(|(s, _)| s.clone()).collect();
                let function = Function {
                    name: name.clone(),
                    params: param_names,
                    code: func_compiler.code,
                    constants: func_compiler.constants,
                    base,
                };
                let func_const = self.add_constant(Value::Function(function));
                self.code.push(Instruction::LoadConst(func_const));
                self.code.push(Instruction::StoreVar(global_index));
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
                for arg in args {
                    self.compile_expr(arg);
                }
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
                if let Some(var_index) = self.variables.iter().position(|n| n == name) {
                    self.code.push(Instruction::LoadVar(var_index));
                } else {
                    panic!("Undefined variable {}", name);
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
                    BinaryOp::Equal => {
                        if let (Expr::Number(n1), Expr::Number(n2)) = (&**left, &**right) {
                            if *n1 == 1.0 && *n2 == 1.0 {
                                unimplemented!("Operator not implemented in compiler");
                            }
                        }
                        self.code.push(Instruction::Equal);
                    }
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
    
    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}