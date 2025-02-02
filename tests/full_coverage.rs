use std::panic;

use moon::{
    ast,
    bytecode::Instruction,
    compiler::Compiler,
    lexer,
    parser,
    value::Value,
    vm::{VM, VMError},
};

#[test]
fn test_vm_add() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Add,
    ];
    let constants = vec![Value::Number(1.0), Value::Number(2.0)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn test_vm_sub() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Sub,
    ];
    let constants = vec![Value::Number(5.0), Value::Number(3.0)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Number(2.0));
}

#[test]
fn test_vm_mul() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Mul,
    ];
    let constants = vec![Value::Number(4.0), Value::Number(3.0)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Number(12.0));
}

#[test]
fn test_vm_division() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Div,
    ];
    let constants = vec![Value::Number(6.0), Value::Number(2.0)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn test_vm_division_by_zero() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Div,
    ];
    let constants = vec![Value::Number(6.0), Value::Number(0.0)];
    let result = vm.run(&instructions, &constants);
    assert_eq!(result, Err(VMError::DivisionByZero));
}

#[test]
fn test_vm_negate() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![Instruction::LoadConst(0), Instruction::Negate];
    let constants = vec![Value::Number(5.0)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Number(-5.0));
}

#[test]
fn test_vm_not() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![Instruction::LoadConst(0), Instruction::Not];
    let constants = vec![Value::Bool(false)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Bool(true));
}

#[test]
fn test_vm_type_error_add() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::LoadConst(1),
        Instruction::Add,
    ];
    let constants = vec![Value::Bool(true), Value::Number(2.0)];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::TypeError(_))));
}

#[test]
fn test_vm_type_error_not() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![Instruction::LoadConst(0), Instruction::Not];
    let constants = vec![Value::Number(1.0)];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::TypeError(_))));
}

#[test]
fn test_vm_load_const_error() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![Instruction::LoadConst(1)];
    let constants = vec![Value::Number(1.0)];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::TypeError(ref s)) if s.contains("No constant at index")));
}

#[test]
fn test_vm_stack_underflow() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![Instruction::Add];
    let constants = vec![];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::StackUnderflow)));
}

#[test]
fn test_vm_variable_load_store() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::StoreVar(0),
        Instruction::LoadVar(0),
    ];
    let constants = vec![Value::Number(42.0)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn test_vm_if_statement() {
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::JumpIfFalse(4),
        Instruction::LoadConst(1),
        Instruction::Jump(5),
        Instruction::LoadConst(2),
    ];
    let constants = vec![Value::Bool(false), Value::Number(1.0), Value::Number(2.0)];
    let mut vm = VM::new(vec![]);
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Number(2.0));
}

#[test]
fn test_vm_function_call() {
    let func_code = vec![
        Instruction::LoadConst(0),
        Instruction::Return,
    ];
    let func_constants = vec![Value::Number(10.0)];
    let function = Value::Function(moon::value::Function {
        name: "const10".to_string(),
        params: vec![],
        code: func_code,
        constants: func_constants,
        base: 0,
    });
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::Call(0, 0),
    ];
    let constants = vec![function];
    let mut vm = VM::new(vec![]);
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Number(10.0));
}

#[test]
fn test_vm_call_non_function() {
    let mut vm = VM::new(vec![]);
    let instructions = vec![
        Instruction::LoadConst(0),
        Instruction::Call(0, 0),
    ];
    let constants = vec![Value::Number(42.0)];
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
        Instruction::LoadConst(0) => {}
        _ => panic!("Expected LoadConst instruction"),
    }
    assert_eq!(compiler.constants, vec![Value::Number(42.0)]);
}

#[test]
fn test_compile_bool() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Bool(true);
    compiler.compile_expr(&expr);
    assert_eq!(compiler.code.len(), 1);
    match compiler.code[0] {
        Instruction::LoadConst(0) => {}
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
        Instruction::LoadConst(0) => {}
        _ => panic!("Expected LoadConst"),
    }
    match compiler.code[1] {
        Instruction::Negate => {}
        _ => panic!("Expected Negate instruction"),
    }
    assert_eq!(compiler.constants, vec![Value::Number(3.0)]);
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
        Instruction::LoadConst(0) => {}
        _ => panic!("Expected LoadConst"),
    }
    match compiler.code[1] {
        Instruction::Not => {}
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
        Instruction::LoadConst(0) => {}
        _ => panic!("Expected LoadConst for left operand"),
    }
    match compiler.code[1] {
        Instruction::LoadConst(1) => {}
        _ => panic!("Expected LoadConst for right operand"),
    }
    match compiler.code[2] {
        Instruction::Add => {}
        _ => panic!("Expected Add instruction"),
    }
    assert_eq!(compiler.constants, vec![Value::Number(1.0), Value::Number(2.0)]);
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
        Instruction::Sub => {}
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
        Instruction::Mul => {}
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
        Instruction::Div => {}
        _ => panic!("Expected Div instruction"),
    }
}

#[test]
#[should_panic(expected = "Operator not implemented in compiler")]
fn test_compile_binary_unimplemented_operator() {
    let mut compiler = Compiler::new();
    let expr = ast::Expr::Binary {
        left: Box::new(ast::Expr::Number(1.0)),
        op: ast::BinaryOp::Equal,
        right: Box::new(ast::Expr::Number(1.0)),
    };
    compiler.compile_expr(&expr);
}

#[test]
fn test_compile_and_run_variable_assignment() {
    use moon::ast::{Expr, Stmt, TypeAnnotation};
    let stmts = vec![
        Stmt::VariableDeclaration {
            name: "x".to_string(),
            var_type: TypeAnnotation::Builtin("number".to_string()),
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
    let mut vm = VM::new(vec![]);
    let result = vm.run(&compiler.code, &compiler.constants).unwrap();
    assert_eq!(result, Value::Number(7.0));
}

#[test]
fn test_compile_and_run_function_declaration_and_call() {
    use moon::ast::{Expr, Stmt, BinaryOp, TypeAnnotation};
    let stmts = vec![
        Stmt::FunctionDeclaration {
            name: "add_one".to_string(),
            params: vec![("n".to_string(), TypeAnnotation::Builtin("number".to_string()))],
            return_type: Some(TypeAnnotation::Builtin("number".to_string())),
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
            var_type: TypeAnnotation::Builtin("number".to_string()),
            initializer: Some(Expr::Call {
                callee: Box::new(Expr::Identifier("add_one".to_string())),
                arguments: vec![Expr::Number(5.0)],
            }),
        },
        Stmt::Expression(Expr::Identifier("result".to_string())),
    ];
    let mut compiler = Compiler::new();
    compiler.compile_program(&stmts);
    let mut vm = VM::new(vec![]);
    let result = vm.run(&compiler.code, &compiler.constants).unwrap();
    assert_eq!(result, Value::Number(6.0));
}

#[test]
fn test_value_display() {
    let num = Value::Number(3.14);
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
    let func = Value::Function(moon::value::Function {
        name: "foo".to_string(),
        params: vec!["a".to_string(), "b".to_string()],
        code: vec![],
        constants: vec![],
        base: 0,
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
        lexer::Token::Identifier("number".to_string()),
        lexer::Token::Eq,
        lexer::Token::Number(10.0),
    ];
    let stmts = parser::parse(&tokens).unwrap();
    match &stmts[0] {
        ast::Stmt::VariableDeclaration { name, var_type, initializer } => {
            assert_eq!(name, "x");
            match var_type {
                ast::TypeAnnotation::Builtin(s) => assert_eq!(s, "number"),
                _ => panic!("Expected builtin type"),
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
fn test_parser_extra_tokens_error() {
    let tokens = vec![
        lexer::Token::Number(1.0),
        lexer::Token::Number(2.0),
    ];
    let err = parser::parse(&tokens).unwrap_err();
    assert!(err.to_string().contains("Extra tokens"));
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