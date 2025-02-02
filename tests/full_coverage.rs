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

//
// VM Tests
//
#[test]
fn test_vm_add() {
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
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
    let mut vm = VM::new();
    let instructions = vec![Instruction::LoadConst(0), Instruction::Negate];
    let constants = vec![Value::Number(5.0)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Number(-5.0));
}

#[test]
fn test_vm_not() {
    let mut vm = VM::new();
    let instructions = vec![Instruction::LoadConst(0), Instruction::Not];
    let constants = vec![Value::Bool(false)];
    let result = vm.run(&instructions, &constants).unwrap();
    assert_eq!(result, Value::Bool(true));
}

#[test]
fn test_vm_type_error_add() {
    let mut vm = VM::new();
    // Trying to add a Bool and a Number should fail.
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
    let mut vm = VM::new();
    // Pushing a number and trying to use Not (which expects a boolean)
    let instructions = vec![Instruction::LoadConst(0), Instruction::Not];
    let constants = vec![Value::Number(1.0)];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::TypeError(_))));
}

#[test]
fn test_vm_load_const_error() {
    let mut vm = VM::new();
    // Attempt to load a constant from a non-existent index.
    let instructions = vec![Instruction::LoadConst(1)];
    let constants = vec![Value::Number(1.0)];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::TypeError(ref s)) if s.contains("No constant at index")));
}

#[test]
fn test_vm_stack_underflow() {
    let mut vm = VM::new();
    // An instruction that pops values without any pushed constants
    let instructions = vec![Instruction::Add];
    let constants = vec![];
    let result = vm.run(&instructions, &constants);
    assert!(matches!(result, Err(VMError::StackUnderflow)));
}

//
// Compiler Tests
//
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
    assert_eq!(compiler.constants, vec![Value::Number(42.0)]);
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
    // Should compile the inner expression (LoadConst) then add a Negate instruction.
    assert_eq!(compiler.code.len(), 2);
    match compiler.code[0] {
        Instruction::LoadConst(0) => {},
        _ => panic!("Expected LoadConst"),
    }
    match compiler.code[1] {
        Instruction::Negate => {},
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
        Instruction::LoadConst(0) => {},
        _ => panic!("Expected LoadConst"),
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
#[should_panic(expected = "Operator not implemented in compiler")]
fn test_compile_binary_unimplemented_operator() {
    let mut compiler = Compiler::new();
    // Using an operator not implemented in the compiler (e.g. Equal)
    let expr = ast::Expr::Binary {
        left: Box::new(ast::Expr::Number(1.0)),
        op: ast::BinaryOp::Equal,
        right: Box::new(ast::Expr::Number(1.0)),
    };
    compiler.compile_expr(&expr);
}

//
// Value and Bytecode Tests
//
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

//
// AST Tests (simply checking Debug formatting here)
//
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

//
// Lexer Tests
//
#[test]
fn test_lexer_tokenize_numbers_and_ops() {
    let input = "123 + 45.67 - (8 * 9) != 0";
    let tokens = lexer::tokenize(input).unwrap();
    let expected = vec![
        lexer::Token::Number(123.0),
        lexer::Token::Plus,
        lexer::Token::Number(45.67),
        lexer::Token::Minus,
        lexer::Token::LParen,
        lexer::Token::Number(8.0),
        lexer::Token::Star,
        lexer::Token::Number(9.0),
        lexer::Token::RParen,
        lexer::Token::NotEq,
        lexer::Token::Number(0.0),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_lexer_tokenize_boolean_and_logical() {
    let input = "true && false || true";
    let tokens = lexer::tokenize(input).unwrap();
    let expected = vec![
        lexer::Token::True,
        lexer::Token::And,
        lexer::Token::False,
        lexer::Token::Or,
        lexer::Token::True,
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_lexer_tokenize_comparison() {
    let input = "< <= > >=";
    let tokens = lexer::tokenize(input).unwrap();
    let expected = vec![
        lexer::Token::Less,
        lexer::Token::LessEq,
        lexer::Token::Greater,
        lexer::Token::GreaterEq,
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_lexer_bang() {
    let tokens = lexer::tokenize("!").unwrap();
    assert_eq!(tokens, vec![lexer::Token::Bang]);
}

#[test]
fn test_lexer_unexpected_token_equals() {
    let err = lexer::tokenize("=").unwrap_err();
    assert_eq!(err.to_string(), "Lexer error: Unexpected token '='. Did you mean '=='?");
}

#[test]
fn test_lexer_unexpected_token_ampersand() {
    let err = lexer::tokenize("&").unwrap_err();
    assert_eq!(err.to_string(), "Lexer error: Expected '&' for '&&'");
}

#[test]
fn test_lexer_unexpected_token_pipe() {
    let err = lexer::tokenize("|").unwrap_err();
    assert_eq!(err.to_string(), "Lexer error: Expected '|' for '||'");
}

#[test]
fn test_lexer_unexpected_identifier() {
    let err = lexer::tokenize("foobar").unwrap_err();
    assert!(err.to_string().contains("Unexpected identifier"));
}

#[test]
fn test_lexer_unexpected_character() {
    let err = lexer::tokenize("@").unwrap_err();
    assert!(err.to_string().contains("Unexpected character"));
}

//
// Parser Tests
//
#[test]
fn test_parser_simple_number() {
    let tokens = vec![lexer::Token::Number(42.0)];
    let expr = parser::parse(&tokens).unwrap();
    match expr {
        ast::Expr::Number(n) => assert_eq!(n, 42.0),
        _ => panic!("Expected a number expression"),
    }
}

#[test]
fn test_parser_bool() {
    let tokens = vec![lexer::Token::True];
    let expr = parser::parse(&tokens).unwrap();
    match expr {
        ast::Expr::Bool(b) => assert!(b),
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
    let expr = parser::parse(&tokens).unwrap();
    if let ast::Expr::Binary { left, op, right } = expr {
        match (*left, *right) {
            (ast::Expr::Number(a), ast::Expr::Number(b)) => {
                assert_eq!(a, 1.0);
                assert_eq!(b, 2.0);
            }
            _ => panic!("Expected number expressions"),
        }
        match op {
            ast::BinaryOp::Add => {},
            _ => panic!("Expected Add operator"),
        }
    } else {
        panic!("Expected a binary expression");
    }
}

#[test]
fn test_parser_operator_precedence() {
    // "1 + 2 * 3" should be parsed as "1 + (2 * 3)"
    let tokens = lexer::tokenize("1 + 2 * 3").unwrap();
    let expr = parser::parse(&tokens).unwrap();
    if let ast::Expr::Binary { left, op, right } = expr {
        // Left should be 1
        match *left {
            ast::Expr::Number(n) => assert_eq!(n, 1.0),
            _ => panic!("Expected a number"),
        }
        // Operator should be Add
        match op {
            ast::BinaryOp::Add => {},
            _ => panic!("Expected Add operator"),
        }
        // Right should be a binary multiplication: 2 * 3
        if let ast::Expr::Binary { left: r_left, op: r_op, right: r_right } = *right {
            match *r_left {
                ast::Expr::Number(n) => assert_eq!(n, 2.0),
                _ => panic!("Expected a number"),
            }
            match r_op {
                ast::BinaryOp::Multiply => {},
                _ => panic!("Expected Multiply operator"),
            }
            match *r_right {
                ast::Expr::Number(n) => assert_eq!(n, 3.0),
                _ => panic!("Expected a number"),
            }
        } else {
            panic!("Expected a binary multiplication expression");
        }
    } else {
        panic!("Expected a binary expression");
    }
}

#[test]
fn test_parser_unary() {
    // Test parsing of a unary negate: "-5"
    let tokens = lexer::tokenize("-5").unwrap();
    let expr = parser::parse(&tokens).unwrap();
    if let ast::Expr::Unary { op, expr: inner } = expr {
        match op {
            ast::UnaryOp::Negate => {},
            _ => panic!("Expected Negate operator"),
        }
        if let ast::Expr::Number(n) = *inner {
            assert_eq!(n, 5.0);
        } else {
            panic!("Expected a number");
        }
    } else {
        panic!("Expected a unary expression");
    }
}

#[test]
fn test_parser_not_unary() {
    // Test parsing of a unary not: "!true"
    let tokens = lexer::tokenize("!true").unwrap();
    let expr = parser::parse(&tokens).unwrap();
    if let ast::Expr::Unary { op, expr: inner } = expr {
        match op {
            ast::UnaryOp::Not => {},
            _ => panic!("Expected Not operator"),
        }
        if let ast::Expr::Bool(b) = *inner {
            // The AST retains the literal "true"; evaluation (which would invert it) is done in the VM.
            assert!(b == true);
        } else {
            panic!("Expected a boolean");
        }
    } else {
        panic!("Expected a unary expression");
    }
}


#[test]
fn test_parser_logical_or() {
    // Test parsing "true || false"
    let tokens = lexer::tokenize("true || false").unwrap();
    let expr = parser::parse(&tokens).unwrap();
    if let ast::Expr::Binary { op, .. } = expr {
        match op {
            ast::BinaryOp::Or => {},
            _ => panic!("Expected Or operator"),
        }
    } else {
        panic!("Expected a binary expression");
    }
}

#[test]
fn test_parser_logical_and() {
    // Test parsing "true && false"
    let tokens = lexer::tokenize("true && false").unwrap();
    let expr = parser::parse(&tokens).unwrap();
    if let ast::Expr::Binary { op, .. } = expr {
        match op {
            ast::BinaryOp::And => {},
            _ => panic!("Expected And operator"),
        }
    } else {
        panic!("Expected a binary expression");
    }
}

#[test]
fn test_parser_equality() {
    // Test parsing "1 == 1"
    let tokens = vec![
        lexer::Token::Number(1.0),
        lexer::Token::EqEq,
        lexer::Token::Number(1.0),
    ];
    let expr = parser::parse(&tokens).unwrap();
    if let ast::Expr::Binary { op, .. } = expr {
        match op {
            ast::BinaryOp::Equal => {},
            _ => panic!("Expected Equal operator"),
        }
    } else {
        panic!("Expected a binary expression");
    }
}

#[test]
fn test_parser_comparison() {
    // Test parsing "1 < 2"
    let tokens = vec![
        lexer::Token::Number(1.0),
        lexer::Token::Less,
        lexer::Token::Number(2.0),
    ];
    let expr = parser::parse(&tokens).unwrap();
    if let ast::Expr::Binary { op, .. } = expr {
        match op {
            ast::BinaryOp::Less => {},
            _ => panic!("Expected Less operator"),
        }
    } else {
        panic!("Expected a binary expression");
    }
}

#[test]
fn test_parser_extra_tokens_error() {
    // Provide a valid expression (a single number) followed by an extra token.
    let tokens = vec![
        lexer::Token::Number(1.0),
        lexer::Token::Number(2.0), // Extra token.
    ];
    let err = parser::parse(&tokens).unwrap_err();
    // The parser produces an error like "Parser error: Extra tokens after expression"
    assert!(err.to_string().contains("Extra tokens"));
}

#[test]
fn test_parser_missing_rparen() {
    // Missing right parenthesis.
    let tokens = vec![
        lexer::Token::LParen,
        lexer::Token::Number(1.0),
        lexer::Token::Plus,
        lexer::Token::Number(2.0),
    ];
    let err = parser::parse(&tokens).unwrap_err();
    assert!(err.to_string().contains("Unexpected end"));
}