use crate::ast::{Expr, UnaryOp, BinaryOp};
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
        while let Some(token) = self.current() {
            let op = match token {
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
        while let Some(token) = self.current() {
            let op = match token {
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
        while let Some(token) = self.current() {
            let op = match token {
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
        while let Some(token) = self.current() {
            let op = match token {
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
        if let Some(token) = self.current().cloned() {
            match token {
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
                _ => self.parse_primary(),
            }
        } else {
            Err(ParserError("Unexpected end in unary expression".into()))
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.current().cloned() {
            match token {
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
                Token::LParen => {
                    self.advance();
                    let expr = self.parse_expression()?;
                    self.expect(&Token::RParen)?;
                    Ok(expr)
                }
                unexpected => Err(ParserError(format!("Unexpected token: {:?}", unexpected))),
            }
        } else {
            Err(ParserError("Unexpected end of tokens in primary expression".into()))
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Expr, ParserError> {
    let mut parser = Parser::new(tokens);
    let expr = parser.parse_expression()?;
    if parser.pos < tokens.len() {
        return Err(ParserError("Extra tokens after expression".into()));
    }
    Ok(expr)
}
