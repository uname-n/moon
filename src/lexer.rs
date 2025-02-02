// src/lexer.rs

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Number(f64),
    True,
    False,
    Identifier(String),
    // Operators and punctuation
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Bang,
    Eq,     // assignment: =
    EqEq,   // equality: ==
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
    Colon,  // for type annotations
    Comma,
    // Keywords
    Fn,
    If,
    Else,
    Return,
    Print,
}

#[derive(Debug)]
pub struct LexerError(pub String);

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lexer error: {}", self.0)
    }
}

impl std::error::Error for LexerError {}

pub fn tokenize(input: &str) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    
    while let Some(&ch) = chars.peek() {
        match ch {
            c if c.is_whitespace() => { chars.next(); }
            '0'..='9' => {
                let mut num_str = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        num_str.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let number = num_str.parse::<f64>()
                    .map_err(|_| LexerError(format!("Invalid number: {}", num_str)))?;
                tokens.push(Token::Number(number));
            }
            '+' => { tokens.push(Token::Plus); chars.next(); }
            '-' => { tokens.push(Token::Minus); chars.next(); }
            '*' => { tokens.push(Token::Star); chars.next(); }
            '/' => { tokens.push(Token::Slash); chars.next(); }
            '(' => { tokens.push(Token::LParen); chars.next(); }
            ')' => { tokens.push(Token::RParen); chars.next(); }
            '{' => { tokens.push(Token::LBrace); chars.next(); }
            '}' => { tokens.push(Token::RBrace); chars.next(); }
            ':' => { tokens.push(Token::Colon); chars.next(); }
            ',' => { tokens.push(Token::Comma); chars.next(); }
            '!' => {
                chars.next();
                if let Some(&'=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::NotEq);
                } else {
                    tokens.push(Token::Bang);
                }
            }
            '=' => {
                chars.next();
                if let Some(&'=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::EqEq);
                } else {
                    tokens.push(Token::Eq);
                }
            }
            '<' => {
                chars.next();
                if let Some(&'=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::LessEq);
                } else {
                    tokens.push(Token::Less);
                }
            }
            '>' => {
                chars.next();
                if let Some(&'=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::GreaterEq);
                } else {
                    tokens.push(Token::Greater);
                }
            }
            '&' => {
                chars.next();
                if let Some(&'&') = chars.peek() {
                    chars.next();
                    tokens.push(Token::And);
                } else {
                    return Err(LexerError("Expected '&' for '&&'".into()));
                }
            }
            '|' => {
                chars.next();
                if let Some(&'|') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Or);
                } else {
                    return Err(LexerError("Expected '|' for '||'".into()));
                }
            }
            c if c.is_alphabetic() => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                // check for keywords:
                match ident.as_str() {
                    "true" => tokens.push(Token::True),
                    "false" => tokens.push(Token::False),
                    "fn" => tokens.push(Token::Fn),
                    "if" => tokens.push(Token::If),
                    "else" => tokens.push(Token::Else),
                    "return" => tokens.push(Token::Return),
                    "print" => tokens.push(Token::Print),
                    _ => tokens.push(Token::Identifier(ident)),
                }
            }
            c => return Err(LexerError(format!("Unexpected character: {}", c))),
        }
    }
    Ok(tokens)
}
