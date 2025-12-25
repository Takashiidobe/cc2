use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Int,
    Return,

    // Identifiers and literals
    Identifier(String),
    IntLiteral(i64),

    // Punctuation
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,

    // Special
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Int => write!(f, "int"),
            Token::Return => write!(f, "return"),
            Token::Identifier(s) => write!(f, "Identifier({})", s),
            Token::IntLiteral(n) => write!(f, "IntLiteral({})", n),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Semicolon => write!(f, ";"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            position: 0,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            if token == Token::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace();

        if self.is_at_end() {
            return Ok(Token::Eof);
        }

        let ch = self.current_char();

        match ch {
            '(' => {
                self.advance();
                Ok(Token::OpenParen)
            }
            ')' => {
                self.advance();
                Ok(Token::CloseParen)
            }
            '{' => {
                self.advance();
                Ok(Token::OpenBrace)
            }
            '}' => {
                self.advance();
                Ok(Token::CloseBrace)
            }
            ';' => {
                self.advance();
                Ok(Token::Semicolon)
            }
            '+' => {
                self.advance();
                Ok(Token::Plus)
            }
            '-' => {
                self.advance();
                Ok(Token::Minus)
            }
            '*' => {
                self.advance();
                Ok(Token::Star)
            }
            '/' => {
                self.advance();
                Ok(Token::Slash)
            }
            _ if ch.is_ascii_digit() => self.read_number(),
            _ if ch.is_ascii_alphabetic() || ch == '_' => self.read_identifier(),
            _ => Err(format!("Unexpected character: '{}'", ch)),
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() && self.current_char().is_whitespace() {
            self.advance();
        }
    }

    fn read_number(&mut self) -> Result<Token, String> {
        let start = self.position;
        while !self.is_at_end() && self.current_char().is_ascii_digit() {
            self.advance();
        }
        let num_str: String = self.input[start..self.position].iter().collect();
        let num = num_str.parse::<i64>()
            .map_err(|_| format!("Invalid number: {}", num_str))?;
        Ok(Token::IntLiteral(num))
    }

    fn read_identifier(&mut self) -> Result<Token, String> {
        let start = self.position;
        while !self.is_at_end() {
            let ch = self.current_char();
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let ident: String = self.input[start..self.position].iter().collect();

        let token = match ident.as_str() {
            "int" => Token::Int,
            "return" => Token::Return,
            _ => Token::Identifier(ident),
        };

        Ok(token)
    }

    fn current_char(&self) -> char {
        self.input[self.position]
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }
}
