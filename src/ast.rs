#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    VariableDeclaration {
        name: String,
        var_type: TypeAnnotation,
        initializer: Option<Expr>,
    },
    Assignment {
        name: String,
        expr: Expr,
    },
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    FunctionDeclaration {
        name: String,
        params: Vec<(String, TypeAnnotation)>,
        return_type: Option<TypeAnnotation>,
        body: Vec<Stmt>,
    },
    Return(Option<Expr>),
    Print(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    Bool(bool),
    Str(String),
    Identifier(String),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum TypeAnnotation {
    Builtin(String),
    Custom(String),
}
