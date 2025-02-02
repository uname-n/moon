// src/ast.rs

#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    // A variable declaration such as: x:number = 42
    VariableDeclaration {
        name: String,
        var_type: TypeAnnotation,
        initializer: Option<Expr>,
    },
    // A simple assignment: x = expr
    Assignment {
        name: String,
        expr: Expr,
    },
    // if (condition) { ... } else { ... }
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    // function declaration: fn name(param:type, ...) { ... }
    FunctionDeclaration {
        name: String,
        params: Vec<(String, TypeAnnotation)>,
        return_type: Option<TypeAnnotation>,
        body: Vec<Stmt>,
    },
    // return expression;
    Return(Option<Expr>),
    // print(expr, expr, ...)
    Print(Vec<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Bool(bool),
    // an identifier reference
    Identifier(String),
    // unary expressions: -x, !x
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    // binary expressions: x + y, x == y, etc.
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    // function call: callee(arg, arg, ...)
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug)]
pub enum TypeAnnotation {
    // Builtin types such as "number" or "bool"
    Builtin(String),
    // Custom types defined by the user (future expansion)
    Custom(String),
}
