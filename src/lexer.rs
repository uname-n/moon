#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Token {
    Number(f64),
    True,
    False,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Bang,
    EqEq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
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
            c if c.is_whitespace() => {
                chars.next();
            }
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
            '+' => {
                tokens.push(Token::Plus);
                chars.next();
            }
            '-' => {
                tokens.push(Token::Minus);
                chars.next();
            }
            '*' => {
                tokens.push(Token::Star);
                chars.next();
            }
            '/' => {
                tokens.push(Token::Slash);
                chars.next();
            }
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
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
                    return Err(LexerError("Unexpected token '='. Did you mean '=='?".into()));
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
                    if c.is_alphanumeric() {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                match ident.as_str() {
                    "true" => tokens.push(Token::True),
                    "false" => tokens.push(Token::False),
                    _ => return Err(LexerError(format!("Unexpected identifier: {}", ident))),
                }
            }
            c => {
                return Err(LexerError(format!("Unexpected character: {}", c)));
            }
        }
    }
    Ok(tokens)
}
