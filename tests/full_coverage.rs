use std::panic;
use std::sync::Arc;
use num_bigint::BigInt;
use moon::{
    ast,
    bytecode::Instruction,
    compiler::Compiler,
    lexer,
    parser,
    value::Value,
    vm::{VM, VMError},
    builtins,
};
use moon::value::Function;

#[test]
fn test_vm_add() {
    let mut vm = VM::new();
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Add,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(1)), Value::Integer(BigInt::from(2))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(3)));
}

#[test]
fn test_vm_sub() {
    let mut vm = VM::new();
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Sub,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(5)), Value::Integer(BigInt::from(3))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(2)));
}

#[test]
fn test_vm_mul() {
    let mut vm = VM::new();
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Mul,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(4)), Value::Integer(BigInt::from(3))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(12)));
}

#[test]
fn test_vm_division() {
    let mut vm = VM::new();
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Div,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(6)), Value::Integer(BigInt::from(2))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(3)));
}

#[test]
fn test_vm_division_by_zero() {
    let mut vm = VM::new();
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Div,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(6)), Value::Integer(BigInt::from(0))];
    let result = vm.run(&instructions, &constants);
    assert_eq!(result, Err(VMError::DivisionByZero));
}

#[test]
fn test_vm_negate() {
    let mut vm = VM::new();
    let mut instructions = vec![Instruction::LoadConst(0), Instruction::Negate];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(5))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(-5)));
}

#[test]
fn test_vm_not() {
    let mut vm = VM::new();
    let mut instructions = vec![Instruction::LoadConst(0), Instruction::Not];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Bool(false)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Bool(true));
}

#[test]
fn test_vm_type_error_add() {
    let mut vm = VM::new();
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Add,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Bool(true), Value::Integer(BigInt::from(2))];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::TypeError(_))));
}

#[test]
fn test_vm_type_error_not() {
    let mut vm = VM::new();
    let mut instructions = vec![Instruction::LoadConst(0), Instruction::Not];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(1))];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::TypeError(_))));
}

#[test]
fn test_vm_load_const_error() {
    let mut vm = VM::new();
    let mut instructions = vec![Instruction::LoadConst(1)];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(1))];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::TypeError(ref s)) if s.contains("No constant at index")));
}

#[test]
fn test_vm_local_variable() {
    let func_code = vec![
        Instruction::LoadConst(0),
        Instruction::StoreLocal(0),
        Instruction::LoadLocal(0),
        Instruction::Return,
    ];
    let func_constants = vec![Value::Integer(BigInt::from(42))];
    let function = Value::Function(Function {
        name: "local_test".to_string(),
        params: vec![],
        code: Arc::new(func_code),
        constants: Arc::new(func_constants),
        base: 0,
        closure: None,
    });
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::Call(0, 0),
    ];
    instructions.push(Instruction::Return);
    let constants = vec![function];
    let mut vm = VM::new();
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(42)));
}

#[test]
fn test_vm_if_statement() {
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::JumpIfFalse(4),
        Instruction::LoadConst(1),
        Instruction::Jump(5),
        Instruction::LoadConst(2),
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Bool(false), Value::Integer(BigInt::from(1)), Value::Integer(BigInt::from(2))];
    let mut vm = VM::new();
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(2)));
}

#[test]
fn test_vm_function_call() {
    let func_code = vec![
        Instruction::LoadConst(0),
        Instruction::Return,
    ];
    let func_constants = vec![Value::Integer(BigInt::from(10))];
    let function = Value::Function(Function {
        name: "const10".to_string(),
        params: vec![],
        code: Arc::new(func_code),
        constants: Arc::new(func_constants),
        base: 0,
        closure: None,
    });
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::Call(0, 0),
    ];
    instructions.push(Instruction::Return);
    let constants = vec![function];
    let mut vm = VM::new();
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(10)));
}

#[test]
fn test_vm_call_non_function() {
    let mut vm = VM::new();
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::Call(0, 0),
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(42))];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::TypeError(_))));
}

#[test]
fn test_compile_number() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Number(42.0);
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 1);
    match compiler.code[0] {
        Instruction::LoadConst(0) => {},
        _ => panic!("Expected LoadConst instruction"),
    }
    assert_eq!(compiler.constants, vec![Value::Integer(BigInt::from(42))]);
}

#[test]
fn test_compile_bool() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Bool(true);
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 1);
    match compiler.code[0] {
        Instruction::LoadConst(0) => {},
        _ => panic!("Expected LoadConst instruction"),
    }
    assert_eq!(compiler.constants, vec![Value::Bool(true)]);
}

#[test]
fn test_compile_unary_negate() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Unary {
        op: ast::UnaryOp::Negate,
        expr: Box::new(ast::Expr::Number(3.0)),
    };
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 2);
    match compiler.code[0] {
        Instruction::LoadConst(0) => {},
        _ => panic!("Expected LoadConst"),
    }
    match compiler.code[1] {
        Instruction::Negate => {},
        _ => panic!("Expected Negate instruction"),
    }
    assert_eq!(compiler.constants, vec![Value::Integer(BigInt::from(3))]);
}

#[test]
fn test_compile_unary_not() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Unary {
        op: ast::UnaryOp::Not,
        expr: Box::new(ast::Expr::Bool(false)),
    };
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 2);
    match compiler.code[0] {
        Instruction::LoadConst(0) => {},
        _ => panic!("Expected LoadConst instruction"),
    }
    match compiler.code[1] {
        Instruction::Not => {},
        _ => panic!("Expected Not instruction"),
    }
    assert_eq!(compiler.constants, vec![Value::Bool(false)]);
}

#[test]
fn test_compile_binary_add() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Binary {
        left: Box::new(ast::Expr::Number(1.0)),
        op: ast::BinaryOp::Add,
        right: Box::new(ast::Expr::Number(2.0)),
    };
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 3);
    match compiler.code[0] {
        Instruction::LoadConst(0) => {},
        _ => panic!("Expected LoadConst for left operand"),
    }
    match compiler.code[1] {
        Instruction::LoadConst(1) => {},
        _ => panic!("Expected LoadConst for right operand"),
    }
    match compiler.code[2] {
        Instruction::Add => {},
        _ => panic!("Expected Add instruction"),
    }
    assert_eq!(compiler.constants, vec![
        Value::Integer(BigInt::from(1)),
        Value::Integer(BigInt::from(2))
    ]);
}

#[test]
fn test_compile_binary_subtract() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Binary {
        left: Box::new(ast::Expr::Number(5.0)),
        op: ast::BinaryOp::Subtract,
        right: Box::new(ast::Expr::Number(3.0)),
    };
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 3);
    match compiler.code[2] {
        Instruction::Sub => {},
        _ => panic!("Expected Sub instruction"),
    }
}

#[test]
fn test_compile_binary_multiply() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Binary {
        left: Box::new(ast::Expr::Number(2.0)),
        op: ast::BinaryOp::Multiply,
        right: Box::new(ast::Expr::Number(3.0)),
    };
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 3);
    match compiler.code[2] {
        Instruction::Mul => {},
        _ => panic!("Expected Mul instruction"),
    }
}

#[test]
fn test_compile_binary_divide() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Binary {
        left: Box::new(ast::Expr::Number(10.0)),
        op: ast::BinaryOp::Divide,
        right: Box::new(ast::Expr::Number(2.0)),
    };
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 3);
    match compiler.code[2] {
        Instruction::Div => {},
        _ => panic!("Expected Div instruction"),
    }
}

#[test]
fn test_compile_and_run_variable_assignment() {
    use moon::ast::{Expr, Stmt, TypeAnnotation};
    let stmts = vec![
        Stmt::VariableDeclaration {
            name: "x".to_string(),
            var_type: TypeAnnotation::Int,
            initializer: Some(Expr::Number(5.0)),
        },
        Stmt::Assignment {
            name: "x".to_string(),
            expr: Expr::Number(7.0),
        },
        Stmt::Expression(Expr::Identifier("x".to_string())),
    ];
    let mut compiler = Compiler::new();
    compiler.compile_program(&stmts);
    let mut vm = VM::new();
    let result = vm.run(&compiler.code, &compiler.constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(7)));
}

#[test]
fn test_compile_and_run_function_declaration_and_call() {
    use moon::ast::{Expr, Stmt, BinaryOp, TypeAnnotation};
    let stmts = vec![
        Stmt::FunctionDeclaration {
            name: "add_one".to_string(),
            params: vec![("n".to_string(), TypeAnnotation::Int)],
            return_type: Some(TypeAnnotation::Int),
            body: vec![
                Stmt::Return(Some(Expr::Binary {
                    left: Box::new(Expr::Identifier("n".to_string())),
                    op: BinaryOp::Add,
                    right: Box::new(Expr::Number(1.0)),
                })),
            ],
        },
        Stmt::VariableDeclaration {
            name: "result".to_string(),
            var_type: TypeAnnotation::Int,
            initializer: Some(Expr::Call {
                callee: Box::new(Expr::Identifier("add_one".to_string())),
                arguments: vec![Expr::Number(5.0)],
            }),
        },
        Stmt::Expression(Expr::Identifier("result".to_string())),
    ];
    let mut compiler = Compiler::new();
    compiler.compile_program(&stmts);
    let mut vm = VM::new();
    let result = vm.run(&compiler.code, &compiler.constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(6)));
}

#[test]
fn test_value_display() {
    let num = Value::Float(3.14);
    let b = Value::Bool(true);
    assert_eq!(num.to_string(), "3.14");
    assert_eq!(b.to_string(), "true");
}

#[test]
fn test_bytecode_debug() {
    let inst = Instruction::LoadConst(2);
    let debug_str = format!("{:?}", inst);
    assert!(debug_str.contains("LoadConst"));
}

#[test]
fn test_value_function_display() {
    let func = Value::Function(Function {
        name: "foo".to_string(),
        params: vec!["a".to_string(), "b".to_string()],
        code: Arc::new(vec![]),
        constants: Arc::new(vec![]),
        base: 0,
        closure: None,
    });
    assert!(func.to_string().contains("<fn foo>"));
}

#[test]
fn test_ast_debug() {
    let expr = ast::Expr::Binary {
        left: Box::new(ast::Expr::Number(1.0)),
        op: ast::BinaryOp::Add,
        right: Box::new(ast::Expr::Number(2.0)),
    };
    let debug = format!("{:?}", expr);
    assert!(debug.contains("Number"));
}

#[test]
fn test_ast_custom_type_annotation() {
    use moon::ast::TypeAnnotation;
    let custom = TypeAnnotation::Custom("MyType".to_string());
    assert_eq!(format!("{:?}", custom), "Custom(\"MyType\")");
}

#[test]
fn test_parser_simple_number() {
    let tokens = vec![lexer::Token::Number(42.0)];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Number(n)) => assert_eq!(*n, 42.0),
        _ => panic!("Expected a number expression"),
    }
}

#[test]
fn test_parser_bool() {
    let tokens = vec![lexer::Token::True];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Bool(b)) => assert!(*b),
        _ => panic!("Expected a boolean expression"),
    }
}

#[test]
fn test_parser_parentheses() {
    let tokens = vec![
        lexer::Token::LParen,
        lexer::Token::Number(1.0),
        lexer::Token::Plus,
        lexer::Token::Number(2.0),
        lexer::Token::RParen,
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Binary { left, op, right }) => {
            match (&**left, &**right) {
                (ast::Expr::Number(a), ast::Expr::Number(b)) => {
                    assert_eq!(*a, 1.0);
                    assert_eq!(*b, 2.0);
                }
                _ => panic!("Expected number expressions"),
            }
            match op {
                ast::BinaryOp::Add => {},
                _ => panic!("Expected Add operator"),
            }
        }
        _ => panic!("Expected a binary expression"),
    }
}

#[test]
fn test_parser_operator_precedence() {
    let tokens = lexer::tokenize("1 + 2 * 3").unwrap();
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Binary { left, op, right }) => {
            match **left {
                ast::Expr::Number(n) => assert_eq!(n, 1.0),
                _ => panic!("Expected a number"),
            }
            match op {
                ast::BinaryOp::Add => {},
                _ => panic!("Expected Add operator"),
            }
            if let ast::Expr::Binary { left: r_left, op: r_op, right: r_right } = &**right {
                match **r_left {
                    ast::Expr::Number(n) => assert_eq!(n, 2.0),
                    _ => panic!("Expected a number"),
                }
                match r_op {
                    ast::BinaryOp::Multiply => {},
                    _ => panic!("Expected Multiply operator"),
                }
                match **r_right {
                    ast::Expr::Number(n) => assert_eq!(n, 3.0),
                    _ => panic!("Expected a number"),
                }
            } else {
                panic!("Expected a binary multiplication expression");
            }
        }
        _ => panic!("Expected a binary expression"),
    }
}

#[test]
fn test_parser_unary() {
    let tokens = lexer::tokenize("-5").unwrap();
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Unary { op, expr: inner }) => {
            match op {
                ast::UnaryOp::Negate => {},
                _ => panic!("Expected Negate operator"),
            }
            if let ast::Expr::Number(n) = &**inner {
                assert_eq!(*n, 5.0);
            } else {
                panic!("Expected a number");
            }
        }
        _ => panic!("Expected a unary expression"),
    }
}

#[test]
fn test_parser_not_unary() {
    let tokens = lexer::tokenize("!true").unwrap();
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Unary { op, expr: inner }) => {
            match op {
                ast::UnaryOp::Not => {},
                _ => panic!("Expected Not operator"),
            }
            if let ast::Expr::Bool(b) = &**inner {
                assert_eq!(*b, true);
            } else {
                panic!("Expected a boolean");
            }
        }
        _ => panic!("Expected a unary expression"),
    }
}

#[test]
fn test_parser_logical_or() {
    let tokens = lexer::tokenize("true || false").unwrap();
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Binary { op, .. }) => {
            match op {
                ast::BinaryOp::Or => {},
                _ => panic!("Expected Or operator"),
            }
        }
        _ => panic!("Expected a binary expression"),
    }
}

#[test]
fn test_parser_logical_and() {
    let tokens = lexer::tokenize("true && false").unwrap();
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Binary { op, .. }) => {
            match op {
                ast::BinaryOp::And => {},
                _ => panic!("Expected And operator"),
            }
        }
        _ => panic!("Expected a binary expression"),
    }
}

#[test]
fn test_parser_equality() {
    let tokens = vec![
        lexer::Token::Number(1.0),
        lexer::Token::EqEq,
        lexer::Token::Number(1.0),
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Binary { op, .. }) => {
            match op {
                ast::BinaryOp::Equal => {},
                _ => panic!("Expected Equal operator"),
            }
        }
        _ => panic!("Expected a binary expression"),
    }
}

#[test]
fn test_parser_comparison() {
    let tokens = vec![
        lexer::Token::Number(1.0),
        lexer::Token::Less,
        lexer::Token::Number(2.0),
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Expression(ast::Expr::Binary { op, .. }) => {
            match op {
                ast::BinaryOp::Less => {},
                _ => panic!("Expected Less operator"),
            }
        }
        _ => panic!("Expected a binary expression"),
    }
}

#[test]
fn test_parser_variable_declaration() {
    let tokens = vec![
        lexer::Token::Identifier("x".to_string()),
        lexer::Token::Colon,
        lexer::Token::Identifier("int".to_string()),
        lexer::Token::Eq,
        lexer::Token::Number(10.0),
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::VariableDeclaration { name, var_type, initializer } => {
            assert_eq!(name, "x");
            match var_type {
                ast::TypeAnnotation::Int => {},
                _ => panic!("Expected int type"),
            }
            match initializer {
                Some(ast::Expr::Number(n)) => assert_eq!(*n, 10.0),
                _ => panic!("Expected number initializer"),
            }
        }
        _ => panic!("Expected variable declaration"),
    }
}

#[test]
fn test_parser_assignment() {
    let tokens = vec![
        lexer::Token::Identifier("x".to_string()),
        lexer::Token::Eq,
        lexer::Token::Number(20.0),
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Assignment { name, expr } => {
            assert_eq!(name, "x");
            match expr {
                ast::Expr::Number(n) => assert_eq!(*n, 20.0),
                _ => panic!("Expected number"),
            }
        }
        _ => panic!("Expected assignment"),
    }
}

#[test]
fn test_parser_if_statement() {
    let tokens = vec![
        lexer::Token::If,
        lexer::Token::True,
        lexer::Token::LBrace,
        lexer::Token::Number(1.0),
        lexer::Token::RBrace,
        lexer::Token::Else,
        lexer::Token::LBrace,
        lexer::Token::Number(2.0),
        lexer::Token::RBrace,
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::If { condition, then_branch, else_branch } => {
            if let ast::Expr::Bool(b) = condition {
                assert_eq!(*b, true);
            } else {
                panic!("Expected boolean condition");
            }
            assert_eq!(then_branch.len(), 1);
            match &then_branch[0] {
                ast::Stmt::Expression(ast::Expr::Number(n)) => assert_eq!(*n, 1.0),
                _ => panic!("Expected number in then branch"),
            }
            let else_branch = else_branch.as_ref().expect("Expected else branch");
            assert_eq!(else_branch.len(), 1);
            match &else_branch[0] {
                ast::Stmt::Expression(ast::Expr::Number(n)) => assert_eq!(*n, 2.0),
                _ => panic!("Expected number in else branch"),
            }
        },
        _ => panic!("Expected if statement"),
    }
}

#[test]
fn test_parser_function_declaration() {
    let tokens = vec![
        lexer::Token::Fn,
        lexer::Token::Identifier("foo".to_string()),
        lexer::Token::LParen,
        lexer::Token::RParen,
        lexer::Token::LBrace,
        lexer::Token::Return,
        lexer::Token::Number(0.0),
        lexer::Token::RBrace,
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::FunctionDeclaration { name, params, body, .. } => {
            assert_eq!(name, "foo");
            assert!(params.is_empty());
            assert_eq!(body.len(), 1);
            match &body[0] {
                ast::Stmt::Return(Some(ast::Expr::Number(n))) => assert_eq!(*n, 0.0),
                _ => panic!("Expected return statement with number 0"),
            }
        },
        _ => panic!("Expected function declaration"),
    }
}

#[test]
fn test_parser_return_statement() {
    let tokens = vec![
        lexer::Token::Return,
        lexer::Token::Number(5.0),
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Return(Some(ast::Expr::Number(n))) => assert_eq!(*n, 5.0),
        _ => panic!("Expected return statement with number 5"),
    }
}

#[test]
fn test_parser_print_statement() {
    let tokens = vec![
        lexer::Token::Print,
        lexer::Token::LParen,
        lexer::Token::Number(42.0),
        lexer::Token::Comma,
        lexer::Token::True,
        lexer::Token::RParen,
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::Print(args) => {
            assert_eq!(args.len(), 2);
            match &args[0] {
                ast::Expr::Number(n) => assert_eq!(*n, 42.0),
                _ => panic!("Expected number as first argument"),
            }
            match &args[1] {
                ast::Expr::Bool(b) => assert_eq!(*b, true),
                _ => panic!("Expected bool as second argument"),
            }
        },
        _ => panic!("Expected print statement"),
    }
}

#[test]
fn test_parser_missing_rparen() {
    let tokens = vec![
        lexer::Token::LParen,
        lexer::Token::Number(1.0),
        lexer::Token::Plus,
        lexer::Token::Number(2.0),
    ];
    let err = parser::parse(&tokens).unwrap_err();
    assert!(err.to_string().contains("Unexpected end"));
}

#[test]
fn test_builtin_type() {
    let args = vec![Value::Integer(BigInt::from(10))];
    let result = builtins::builtin_type(&args).unwrap();
    assert_eq!(result, Value::Str("int".to_string()));
}

#[test]
fn test_builtin_print() {
    let args = vec![Value::Str("hello".to_string())];
    let result = builtins::builtin_print(&args).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(0)));
}

#[test]
fn test_register_builtins() {
    let mut vm = VM::new();
    builtins::register_builtins(&mut vm);
    if let Some(Value::BuiltinFunction(name, _)) = vm.globals.get("print") {
        assert_eq!(name, "print");
    } else {
        panic!("Builtin 'print' not registered correctly");
    }
    if let Some(Value::BuiltinFunction(name, _)) = vm.globals.get("type") {
        assert_eq!(name, "type");
    } else {
        panic!("Builtin 'type' not registered correctly");
    }
}

#[test]
fn test_compile_and_run_print_statement() {
    use moon::ast::{Expr, Stmt};
    let stmts = vec![
        Stmt::Print(vec![Expr::Number(123.0)]),
    ];
    let mut compiler = Compiler::new();
    compiler.compile_program(&stmts);
    let mut vm = VM::new();
    builtins::register_builtins(&mut vm);
    let result = vm.run(&compiler.code, &compiler.constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(0)));
}

#[test]
fn test_top_level_compiler_global_declaration_instructions() {
    use moon::ast::{Stmt, TypeAnnotation, Expr};
    let mut compiler = Compiler::new();
    let stmt = Stmt::VariableDeclaration {
        name: "a".to_string(),
        var_type: TypeAnnotation::Int,
        initializer: Some(Expr::Number(10.0)),
    };
    compiler.compile_stmt(&stmt);
    if let Some(last) = compiler.code.last() {
        match last {
            Instruction::StoreGlobal(name) => assert_eq!(name, "a"),
            _ => panic!("Expected StoreGlobal instruction"),
        }
    } else {
        panic!("No instructions generated");
    }
}

#[test]
fn test_top_level_compiler_assignment_instructions() {
    use moon::ast::{Stmt, Expr};
    let mut compiler = Compiler::new();
    let stmt = Stmt::Assignment {
        name: "b".to_string(),
        expr: Expr::Number(20.0),
    };
    compiler.compile_stmt(&stmt);
    if let Some(last) = compiler.code.last() {
        match last {
            Instruction::StoreGlobal(name) => assert_eq!(name, "b"),
            _ => panic!("Expected StoreGlobal instruction"),
        }
    } else {
        panic!("No instructions generated");
    }
}

#[test]
fn test_vm_tail_call_user_function() {
    let g_instructions = vec![
        Instruction::LoadConst(0),
        Instruction::Return,
    ];
    let g_constants = vec![Value::Integer(BigInt::from(99))];
    let function_g = Value::Function(Function {
        name: "g".to_string(),
        params: vec![],
        code: Arc::new(g_instructions),
        constants: Arc::new(g_constants),
        base: 0,
        closure: None,
    });
    let f_instructions = vec![
        Instruction::LoadConst(0),
        Instruction::TailCall(0, 0),
    ];
    let f_constants = vec![function_g.clone()];
    let function_f = Value::Function(Function {
        name: "f".to_string(),
        params: vec![],
        code: Arc::new(f_instructions),
        constants: Arc::new(f_constants),
        base: 0,
        closure: None,
    });
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::Call(0, 0),
    ];
    instructions.push(Instruction::Return);
    let constants = vec![function_f];
    let mut vm = VM::new();
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(99)));
}

#[test]
fn test_vm_tail_call_builtin_function() {
    let builtin_dummy = Value::BuiltinFunction("dummy".to_string(), |args: &[Value]| -> Result<Value, VMError> {
        Ok(Value::Integer(BigInt::from(123)))
    });
    let f_instructions = vec![
        Instruction::LoadConst(0),
        Instruction::TailCall(0, 0),
    ];
    let f_constants = vec![builtin_dummy.clone()];
    let function_f = Value::Function(Function {
        name: "f".to_string(),
        params: vec![],
        code: Arc::new(f_instructions),
        constants: Arc::new(f_constants),
        base: 0,
        closure: None,
    });
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::Call(0, 0),
    ];
    instructions.push(Instruction::Return);
    let constants = vec![function_f];
    let mut vm = VM::new();
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(123)));
}

#[test]
fn test_compile_and_run_while_loop() {
    use moon::ast::{Stmt, Expr, BinaryOp, TypeAnnotation};
    let stmts = vec![
        Stmt::VariableDeclaration {
            name: "x".to_string(),
            var_type: TypeAnnotation::Int,
            initializer: Some(Expr::Number(0.0)),
        },
        Stmt::While {
            condition: Expr::Binary {
                left: Box::new(Expr::Identifier("x".to_string())),
                op: BinaryOp::Less,
                right: Box::new(Expr::Number(3.0)),
            },
            body: vec![
                Stmt::Assignment {
                    name: "x".to_string(),
                    expr: Expr::Binary {
                        left: Box::new(Expr::Identifier("x".to_string())),
                        op: BinaryOp::Add,
                        right: Box::new(Expr::Number(1.0)),
                    },
                },
            ],
        },
        Stmt::Expression(Expr::Identifier("x".to_string())),
    ];
    let mut compiler = Compiler::new();
    compiler.compile_program(&stmts);
    let mut vm = VM::new();
    let result = vm.run(&compiler.code, &compiler.constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(3)));
}

#[test]
fn test_compile_string_literal() {
    let mut compiler = Compiler::new();
    let expr = moon::ast::Expr::Str("hello".to_string());
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 1);
    match compiler.code[0] {
        Instruction::LoadConst(0) => {},
        _ => panic!("Expected LoadConst instruction"),
    }
    assert_eq!(compiler.constants, vec![Value::Str("hello".to_string())]);
}

#[test]
fn test_vm_closure() {
    use std::collections::HashMap;
    let mut c = HashMap::new();
    c.insert("a".to_string(), Value::Integer(BigInt::from(777)));
    let instructions = vec![
        Instruction::LoadClosure("a".to_string()),
        Instruction::Return,
    ];
    let constants = vec![];
    let function = Value::Function(Function {
        name: "closure_test".to_string(),
        params: vec![],
        code: Arc::new(instructions),
        constants: Arc::new(constants),
        base: 0,
        closure: Some(c),
    });
    let mut call_instructions = vec![
        Instruction::LoadConst(0),
        Instruction::Call(0, 0),
    ];
    call_instructions.push(Instruction::Return);
    let call_constants = vec![function];
    let mut vm = VM::new();
    let result = vm.run(&call_instructions, &call_constants).unwrap();
    assert_eq!(result, Value::Integer(BigInt::from(777)));
}

#[test]
fn test_vm_comparisons() {
    let mut vm = VM::new();
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Less,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(3)), Value::Integer(BigInt::from(5))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Bool(true));
    
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Greater,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(3)), Value::Integer(BigInt::from(5))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Bool(false));
    
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::LessEqual,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(3)), Value::Integer(BigInt::from(3))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Bool(true));
    
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::GreaterEqual,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(3)), Value::Integer(BigInt::from(5))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Bool(false));
    
    let mut instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::NotEqual,
    ];
    instructions.push(Instruction::Return);
    let constants = vec![Value::Integer(BigInt::from(3)), Value::Integer(BigInt::from(5))];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Bool(true));
}