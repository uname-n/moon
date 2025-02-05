use crate::ast::{BinaryOp, Expr, Stmt, TypeAnnotation, UnaryOp};
use crate::lexer::Token;

#[derive(Debug)]
pub struct ParserError(pub String);

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parser error: {}", self.0)
    }
}

impl std::error::Error for ParserError {}

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn expect(&mut self, expected: &Token) -> Result<(), ParserError>
    where
        Token: PartialEq,
    {
        if let Some(token) = self.current() {
            if token == expected {
                self.advance();
                Ok(())
            } else {
                Err(ParserError(format!("Expected {:?}, got {:?}", expected, token)))
            }
        } else {
            Err(ParserError("Unexpected end of tokens".into()))
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();
        while self.pos < self.tokens.len() {
            let st = self.parse_statement()?;
            stmts.push(st);
        }
        Ok(stmts)
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        match self.current() {
            Some(Token::Fn) => self.parse_function_declaration(),
            Some(Token::If) => self.parse_if_statement(),
            Some(Token::While) => self.parse_while_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::Print) => self.parse_print_statement(),
            Some(Token::Identifier(_)) => {
                if self.pos + 1 < self.tokens.len() {
                    if let Token::Colon = self.tokens[self.pos + 1] {
                        self.parse_variable_declaration()
                    } else if let Token::Eq = self.tokens[self.pos + 1] {
                        self.parse_assignment()
                    } else {
                        self.parse_expression_statement()
                    }
                } else {
                    self.parse_expression_statement()
                }
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = if let Some(Token::Identifier(n)) = self.current() {
            let s = n.clone();
            self.advance();
            s
        } else {
            return Err(ParserError("Expected identifier in variable declaration".into()));
        };
        self.expect(&Token::Colon)?;
        let type_str = if let Some(Token::Identifier(t)) = self.current() {
            let s = t.clone();
            self.advance();
            s
        } else {
            return Err(ParserError("Expected type identifier after ':'".into()));
        };
        let var_type = match type_str.as_str() {
            "int" => TypeAnnotation::Int,
            "float" => TypeAnnotation::Float,
            other => TypeAnnotation::Builtin(other.to_string()),
        };
        let initializer = if let Some(Token::Eq) = self.current() {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok(Stmt::VariableDeclaration { name, var_type, initializer })
    }

    fn parse_assignment(&mut self) -> Result<Stmt, ParserError> {
        let name = if let Some(Token::Identifier(n)) = self.current() {
            let s = n.clone();
            self.advance();
            s
        } else {
            return Err(ParserError("Expected identifier in assignment".into()));
        };
        self.expect(&Token::Eq)?;
        let expr = self.parse_expression()?;
        Ok(Stmt::Assignment { name, expr })
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::If)?;
        let condition = self.parse_expression()?;
        let then_branch = self.parse_block()?;
        let else_branch = if let Some(Token::Else) = self.current() {
            self.advance();
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Stmt::If { condition, then_branch, else_branch })
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::While)?;
        self.expect(&Token::LParen)?;
        let condition = self.parse_expression()?;
        self.expect(&Token::RParen)?;
        let body = self.parse_block()?;
        Ok(Stmt::While { condition, body })
    }

    fn parse_function_declaration(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::Fn)?;
        let name = if let Some(Token::Identifier(n)) = self.current() {
            let s = n.clone();
            self.advance();
            s
        } else {
            return Err(ParserError("Expected function name".into()));
        };
        self.expect(&Token::LParen)?;
        let mut params = Vec::new();
        if let Some(Token::Identifier(_)) = self.current() {
            loop {
                let param_name = if let Some(Token::Identifier(n)) = self.current() {
                    let s = n.clone();
                    self.advance();
                    s
                } else {
                    return Err(ParserError("Expected parameter name".into()));
                };
                self.expect(&Token::Colon)?;
                let type_str = if let Some(Token::Identifier(t)) = self.current() {
                    let s = t.clone();
                    self.advance();
                    s
                } else {
                    return Err(ParserError("Expected parameter type".into()));
                };
                let param_type = match type_str.as_str() {
                    "int" => TypeAnnotation::Int,
                    "float" => TypeAnnotation::Float,
                    other => TypeAnnotation::Builtin(other.to_string()),
                };
                params.push((param_name, param_type));
                if let Some(Token::Comma) = self.current() {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(&Token::RParen)?;
        let body = self.parse_block()?;
        Ok(Stmt::FunctionDeclaration { name, params, return_type: None, body })
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::Return)?;
        let expr = Some(self.parse_expression()?);
        Ok(Stmt::Return(expr))
    }

    fn parse_print_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::Print)?;
        self.expect(&Token::LParen)?;
        let mut args = Vec::new();
        if let Some(Token::RParen) = self.current() {
        } else {
            loop {
                args.push(self.parse_expression()?);
                if let Some(Token::Comma) = self.current() {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(&Token::RParen)?;
        Ok(Stmt::Print(args))
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.parse_expression()?;
        Ok(Stmt::Expression(expr))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        self.expect(&Token::LBrace)?;
        let mut stmts = Vec::new();
        while let Some(tok) = self.current() {
            if tok == &Token::RBrace {
                break;
            }
            stmts.push(self.parse_statement()?);
        }
        self.expect(&Token::RBrace)?;
        Ok(stmts)
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_and()?;
        while let Some(Token::Or) = self.current() {
            self.advance();
            let right = self.parse_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::Or,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_equality()?;
        while let Some(Token::And) = self.current() {
            self.advance();
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::And,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_comparison()?;
        while let Some(tok) = self.current() {
            let op = match tok {
                Token::EqEq => BinaryOp::Equal,
                Token::NotEq => BinaryOp::NotEqual,
                _ => break,
            };
            self.advance();
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_term()?;
        while let Some(tok) = self.current() {
            let op = match tok {
                Token::Less => BinaryOp::Less,
                Token::LessEq => BinaryOp::LessEqual,
                Token::Greater => BinaryOp::Greater,
                Token::GreaterEq => BinaryOp::GreaterEqual,
                _ => break,
            };
            self.advance();
            let right = self.parse_term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_factor()?;
        while let Some(tok) = self.current() {
            let op = match tok {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Subtract,
                _ => break,
            };
            self.advance();
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_unary()?;
        while let Some(tok) = self.current() {
            let op = match tok {
                Token::Star => BinaryOp::Multiply,
                Token::Slash => BinaryOp::Divide,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(tok) = self.current().cloned() {
            match tok {
                Token::Minus => {
                    self.advance();
                    let expr = self.parse_unary()?;
                    Ok(Expr::Unary {
                        op: UnaryOp::Negate,
                        expr: Box::new(expr),
                    })
                }
                Token::Bang => {
                    self.advance();
                    let expr = self.parse_unary()?;
                    Ok(Expr::Unary {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    })
                }
                _ => self.parse_call(),
            }
        } else {
            Err(ParserError("Unexpected end in unary expression".into()))
        }
    }

    fn parse_call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_primary()?;
        while let Some(Token::LParen) = self.current() {
            self.advance();
            let mut args = Vec::new();
            if let Some(Token::RParen) = self.current() {
            } else {
                loop {
                    args.push(self.parse_expression()?);
                    if let Some(Token::Comma) = self.current() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            self.expect(&Token::RParen)?;
            expr = Expr::Call {
                callee: Box::new(expr),
                arguments: args,
            };
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(tok) = self.current().cloned() {
            match tok {
                Token::Number(n) => {
                    self.advance();
                    Ok(Expr::Number(n))
                }
                Token::True => {
                    self.advance();
                    Ok(Expr::Bool(true))
                }
                Token::False => {
                    self.advance();
                    Ok(Expr::Bool(false))
                }
                Token::StringLiteral(s) => {
                    self.advance();
                    Ok(Expr::Str(s))
                }
                Token::Identifier(s) => {
                    self.advance();
                    Ok(Expr::Identifier(s))
                }
                Token::LParen => {
                    self.advance();
                    let expr = self.parse_expression()?;
                    self.expect(&Token::RParen)?;
                    Ok(expr)
                }
                _ => Err(ParserError(format!("Unexpected token in primary: {:?}", tok))),
            }
        } else {
            Err(ParserError("Unexpected end of tokens in primary expression".into()))
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, ParserError> {
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse_program()?;
    if parser.pos < tokens.len() {
        return Err(ParserError("Extra tokens after program".into()));
    }
    Ok(stmts)
}