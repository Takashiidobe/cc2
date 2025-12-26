use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Int,
    Return,
    If,
    Else,
    While,
    For,
    Struct,
    Union,
    Enum,
    Sizeof,
    Char,
    Unsigned,
    Short,
    Long,

    // Identifiers and literals
    Identifier(String),
    IntLiteral(i64),

    // Punctuation
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Comma,
    OpenBracket,
    CloseBracket,
    Dot,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equals,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    PercentEquals,
    AmpersandEquals,
    Pipe,
    PipeEquals,
    Caret,
    CaretEquals,
    LessLessEquals,
    GreaterGreaterEquals,
    LessLess,
    GreaterGreater,
    Tilde,

    // Comparison operators
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    NotEqual,

    // Logical operators
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Ampersand,
    Arrow,

    // Ternary operator
    Question,
    Colon,

    // Special
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Int => write!(f, "int"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::For => write!(f, "for"),
            Token::Struct => write!(f, "struct"),
            Token::Union => write!(f, "union"),
            Token::Enum => write!(f, "enum"),
            Token::Sizeof => write!(f, "sizeof"),
            Token::Char => write!(f, "char"),
            Token::Unsigned => write!(f, "unsigned"),
            Token::Short => write!(f, "short"),
            Token::Long => write!(f, "long"),
            Token::Identifier(s) => write!(f, "Identifier({})", s),
            Token::IntLiteral(n) => write!(f, "IntLiteral({})", n),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::Dot => write!(f, "."),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Equals => write!(f, "="),
            Token::PlusEquals => write!(f, "+="),
            Token::MinusEquals => write!(f, "-="),
            Token::StarEquals => write!(f, "*="),
            Token::SlashEquals => write!(f, "/="),
            Token::PercentEquals => write!(f, "%="),
            Token::AmpersandEquals => write!(f, "&="),
            Token::Pipe => write!(f, "|"),
            Token::PipeEquals => write!(f, "|="),
            Token::Caret => write!(f, "^"),
            Token::CaretEquals => write!(f, "^="),
            Token::LessLessEquals => write!(f, "<<="),
            Token::GreaterGreaterEquals => write!(f, ">>="),
            Token::LessLess => write!(f, "<<"),
            Token::GreaterGreater => write!(f, ">>"),
            Token::Tilde => write!(f, "~"),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),
            Token::EqualEqual => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalOr => write!(f, "||"),
            Token::LogicalNot => write!(f, "!"),
            Token::Ampersand => write!(f, "&"),
            Token::Arrow => write!(f, "->"),
            Token::Question => write!(f, "?"),
            Token::Colon => write!(f, ":"),
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
            ',' => {
                self.advance();
                Ok(Token::Comma)
            }
            '.' => {
                self.advance();
                Ok(Token::Dot)
            }
            '[' => {
                self.advance();
                Ok(Token::OpenBracket)
            }
            ']' => {
                self.advance();
                Ok(Token::CloseBracket)
            }
            '=' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::EqualEqual)
                } else {
                    Ok(Token::Equals)
                }
            }
            '<' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::LessEqual)
                } else if !self.is_at_end() && self.current_char() == '<' {
                    self.advance();
                    if !self.is_at_end() && self.current_char() == '=' {
                        self.advance();
                        Ok(Token::LessLessEquals)
                    } else {
                        Ok(Token::LessLess)
                    }
                } else {
                    Ok(Token::Less)
                }
            }
            '>' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::GreaterEqual)
                } else if !self.is_at_end() && self.current_char() == '>' {
                    self.advance();
                    if !self.is_at_end() && self.current_char() == '=' {
                        self.advance();
                        Ok(Token::GreaterGreaterEquals)
                    } else {
                        Ok(Token::GreaterGreater)
                    }
                } else {
                    Ok(Token::Greater)
                }
            }
            '!' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::NotEqual)
                } else {
                    Ok(Token::LogicalNot)
                }
            }
            '&' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '&' {
                    self.advance();
                    Ok(Token::LogicalAnd)
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::AmpersandEquals)
                } else {
                    Ok(Token::Ampersand)
                }
            }
            '|' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '|' {
                    self.advance();
                    Ok(Token::LogicalOr)
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::PipeEquals)
                } else {
                    Ok(Token::Pipe)
                }
            }
            '+' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::PlusEquals)
                } else {
                    Ok(Token::Plus)
                }
            }
            '-' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '>' {
                    self.advance();
                    Ok(Token::Arrow)
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::MinusEquals)
                } else {
                    Ok(Token::Minus)
                }
            }
            '*' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::StarEquals)
                } else {
                    Ok(Token::Star)
                }
            }
            '/' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::SlashEquals)
                } else {
                    Ok(Token::Slash)
                }
            }
            '%' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::PercentEquals)
                } else {
                    Ok(Token::Percent)
                }
            }
            '^' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(Token::CaretEquals)
                } else {
                    Ok(Token::Caret)
                }
            }
            '~' => {
                self.advance();
                Ok(Token::Tilde)
            }
            '?' => {
                self.advance();
                Ok(Token::Question)
            }
            ':' => {
                self.advance();
                Ok(Token::Colon)
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
        let num = num_str
            .parse::<i64>()
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
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "struct" => Token::Struct,
            "union" => Token::Union,
            "enum" => Token::Enum,
            "sizeof" => Token::Sizeof,
            "char" => Token::Char,
            "unsigned" => Token::Unsigned,
            "short" => Token::Short,
            "long" => Token::Long,
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
