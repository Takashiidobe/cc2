use std::fmt;

/// Represents a location in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    pub fn new(line: usize, column: usize) -> Self {
        SourceLocation { line, column }
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A token with its location in the source code
#[derive(Debug, Clone, PartialEq)]
pub struct LocatedToken {
    pub token: Token,
    pub location: SourceLocation,
}

impl LocatedToken {
    pub fn new(token: Token, location: SourceLocation) -> Self {
        LocatedToken { token, location }
    }
}

impl fmt::Display for LocatedToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at {}", self.token, self.location)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Int,
    Return,
    If,
    Else,
    While,
    Do,
    For,
    Struct,
    Union,
    Enum,
    Sizeof,
    Char,
    Unsigned,
    Short,
    Long,
    Break,
    Continue,
    Void,

    // Identifiers and literals
    Identifier(String),
    IntLiteral(i64),
    StringLiteral(String),
    CharLiteral(i64),

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
    PlusPlus,
    MinusMinus,
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
            Token::Do => write!(f, "do"),
            Token::For => write!(f, "for"),
            Token::Struct => write!(f, "struct"),
            Token::Union => write!(f, "union"),
            Token::Enum => write!(f, "enum"),
            Token::Sizeof => write!(f, "sizeof"),
            Token::Char => write!(f, "char"),
            Token::Unsigned => write!(f, "unsigned"),
            Token::Short => write!(f, "short"),
            Token::Long => write!(f, "long"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Void => write!(f, "void"),
            Token::Identifier(s) => write!(f, "Identifier({})", s),
            Token::IntLiteral(n) => write!(f, "IntLiteral({})", n),
            Token::StringLiteral(s) => write!(f, "StringLiteral(\"{}\")", s),
            Token::CharLiteral(c) => write!(f, "CharLiteral({})", c),
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
            Token::PlusPlus => write!(f, "++"),
            Token::MinusMinus => write!(f, "--"),
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
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    /// Get the current location in the source
    fn current_location(&self) -> SourceLocation {
        SourceLocation::new(self.line, self.column)
    }

    pub fn tokenize(&mut self) -> Result<Vec<LocatedToken>, String> {
        let mut tokens = Vec::new();

        loop {
            let located_token = self.next_token()?;
            let is_eof = located_token.token == Token::Eof;
            tokens.push(located_token);
            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<LocatedToken, String> {
        self.skip_whitespace();

        if self.is_at_end() {
            return Ok(LocatedToken::new(Token::Eof, self.current_location()));
        }

        let location = self.current_location();
        let ch = self.current_char();

        match ch {
            '(' => {
                self.advance();
                Ok(LocatedToken::new(Token::OpenParen, location))
            }
            ')' => {
                self.advance();
                Ok(LocatedToken::new(Token::CloseParen, location))
            }
            '{' => {
                self.advance();
                Ok(LocatedToken::new(Token::OpenBrace, location))
            }
            '}' => {
                self.advance();
                Ok(LocatedToken::new(Token::CloseBrace, location))
            }
            ';' => {
                self.advance();
                Ok(LocatedToken::new(Token::Semicolon, location))
            }
            ',' => {
                self.advance();
                Ok(LocatedToken::new(Token::Comma, location))
            }
            '.' => {
                self.advance();
                Ok(LocatedToken::new(Token::Dot, location))
            }
            '[' => {
                self.advance();
                Ok(LocatedToken::new(Token::OpenBracket, location))
            }
            ']' => {
                self.advance();
                Ok(LocatedToken::new(Token::CloseBracket, location))
            }
            '=' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::EqualEqual, location))
                } else {
                    Ok(LocatedToken::new(Token::Equals, location))
                }
            }
            '<' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::LessEqual, location))
                } else if !self.is_at_end() && self.current_char() == '<' {
                    self.advance();
                    if !self.is_at_end() && self.current_char() == '=' {
                        self.advance();
                        Ok(LocatedToken::new(Token::LessLessEquals, location))
                    } else {
                        Ok(LocatedToken::new(Token::LessLess, location))
                    }
                } else {
                    Ok(LocatedToken::new(Token::Less, location))
                }
            }
            '>' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::GreaterEqual, location))
                } else if !self.is_at_end() && self.current_char() == '>' {
                    self.advance();
                    if !self.is_at_end() && self.current_char() == '=' {
                        self.advance();
                        Ok(LocatedToken::new(Token::GreaterGreaterEquals, location))
                    } else {
                        Ok(LocatedToken::new(Token::GreaterGreater, location))
                    }
                } else {
                    Ok(LocatedToken::new(Token::Greater, location))
                }
            }
            '!' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::NotEqual, location))
                } else {
                    Ok(LocatedToken::new(Token::LogicalNot, location))
                }
            }
            '&' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '&' {
                    self.advance();
                    Ok(LocatedToken::new(Token::LogicalAnd, location))
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::AmpersandEquals, location))
                } else {
                    Ok(LocatedToken::new(Token::Ampersand, location))
                }
            }
            '|' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '|' {
                    self.advance();
                    Ok(LocatedToken::new(Token::LogicalOr, location))
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::PipeEquals, location))
                } else {
                    Ok(LocatedToken::new(Token::Pipe, location))
                }
            }
            '+' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '+' {
                    self.advance();
                    Ok(LocatedToken::new(Token::PlusPlus, location))
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::PlusEquals, location))
                } else {
                    Ok(LocatedToken::new(Token::Plus, location))
                }
            }
            '-' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '-' {
                    self.advance();
                    Ok(LocatedToken::new(Token::MinusMinus, location))
                } else if !self.is_at_end() && self.current_char() == '>' {
                    self.advance();
                    Ok(LocatedToken::new(Token::Arrow, location))
                } else if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::MinusEquals, location))
                } else {
                    Ok(LocatedToken::new(Token::Minus, location))
                }
            }
            '*' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::StarEquals, location))
                } else {
                    Ok(LocatedToken::new(Token::Star, location))
                }
            }
            '/' => {
                self.advance();
                if !self.is_at_end() {
                    match self.current_char() {
                        '=' => {
                            self.advance();
                            Ok(LocatedToken::new(Token::SlashEquals, location))
                        }
                        '/' => {
                            // C++ style comment - skip to end of line
                            self.skip_line_comment();
                            self.next_token()
                        }
                        '*' => {
                            // C style comment - skip to */
                            self.skip_block_comment()?;
                            self.next_token()
                        }
                        _ => Ok(LocatedToken::new(Token::Slash, location)),
                    }
                } else {
                    Ok(LocatedToken::new(Token::Slash, location))
                }
            }
            '%' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::PercentEquals, location))
                } else {
                    Ok(LocatedToken::new(Token::Percent, location))
                }
            }
            '^' => {
                self.advance();
                if !self.is_at_end() && self.current_char() == '=' {
                    self.advance();
                    Ok(LocatedToken::new(Token::CaretEquals, location))
                } else {
                    Ok(LocatedToken::new(Token::Caret, location))
                }
            }
            '~' => {
                self.advance();
                Ok(LocatedToken::new(Token::Tilde, location))
            }
            '?' => {
                self.advance();
                Ok(LocatedToken::new(Token::Question, location))
            }
            ':' => {
                self.advance();
                Ok(LocatedToken::new(Token::Colon, location))
            }
            '\'' => self.read_char_literal(location),
            '"' => self.read_string_literal(location),
            _ if ch.is_ascii_digit() => self.read_number(location),
            _ if ch.is_ascii_alphabetic() || ch == '_' => self.read_identifier(location),
            _ => Err(format!("Unexpected character: '{}'", ch)),
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() && self.current_char().is_whitespace() {
            self.advance();
        }
    }

    fn skip_line_comment(&mut self) {
        // Skip the second '/' (we already advanced past the first one)
        self.advance();
        // Skip until end of line or end of file
        while !self.is_at_end() && self.current_char() != '\n' {
            self.advance();
        }
        // Don't skip the newline itself - let skip_whitespace handle it
    }

    fn skip_block_comment(&mut self) -> Result<(), String> {
        // Skip the '*' (we already advanced past the '/')
        self.advance();

        // Keep track of where comment started for error reporting
        let mut found_end = false;

        while !self.is_at_end() {
            if self.current_char() == '*' {
                self.advance();
                if !self.is_at_end() && self.current_char() == '/' {
                    self.advance();
                    found_end = true;
                    break;
                }
            } else {
                self.advance();
            }
        }

        if !found_end {
            return Err("Unterminated block comment".to_string());
        }

        Ok(())
    }

    fn read_number(&mut self, location: SourceLocation) -> Result<LocatedToken, String> {
        let start = self.position;
        while !self.is_at_end() && self.current_char().is_ascii_digit() {
            self.advance();
        }
        let num_str: String = self.input[start..self.position].iter().collect();
        let num = num_str
            .parse::<i64>()
            .map_err(|_| format!("Invalid number: {}", num_str))?;
        Ok(LocatedToken::new(Token::IntLiteral(num), location))
    }

    fn read_identifier(&mut self, location: SourceLocation) -> Result<LocatedToken, String> {
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
            "do" => Token::Do,
            "for" => Token::For,
            "struct" => Token::Struct,
            "union" => Token::Union,
            "enum" => Token::Enum,
            "sizeof" => Token::Sizeof,
            "char" => Token::Char,
            "unsigned" => Token::Unsigned,
            "short" => Token::Short,
            "long" => Token::Long,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "void" => Token::Void,
            _ => Token::Identifier(ident),
        };

        Ok(LocatedToken::new(token, location))
    }

    fn read_string_literal(&mut self, location: SourceLocation) -> Result<LocatedToken, String> {
        self.advance(); // Skip opening quote
        let mut value = String::new();

        while !self.is_at_end() && self.current_char() != '"' {
            if self.current_char() == '\\' {
                self.advance();
                if self.is_at_end() {
                    return Err("Unterminated string literal".to_string());
                }
                let escaped = self.parse_escape_sequence()?;
                value.push(escaped);
            } else {
                value.push(self.current_char());
                self.advance();
            }
        }

        if self.is_at_end() {
            return Err("Unterminated string literal".to_string());
        }

        self.advance(); // Skip closing quote
        Ok(LocatedToken::new(Token::StringLiteral(value), location))
    }

    fn read_char_literal(&mut self, location: SourceLocation) -> Result<LocatedToken, String> {
        self.advance(); // Skip opening quote

        if self.is_at_end() {
            return Err("Unterminated character literal".to_string());
        }

        let value = if self.current_char() == '\\' {
            // Handle escape sequences
            self.advance();
            if self.is_at_end() {
                return Err("Unterminated character literal".to_string());
            }
            self.parse_escape_sequence()?
        } else {
            let ch = self.current_char();
            self.advance();
            ch
        };

        if self.is_at_end() || self.current_char() != '\'' {
            return Err("Unterminated character literal".to_string());
        }

        self.advance(); // Skip closing quote
        Ok(LocatedToken::new(
            Token::CharLiteral(value as i64),
            location,
        ))
    }

    fn parse_escape_sequence(&mut self) -> Result<char, String> {
        let ch = self.current_char();
        match ch {
            // Basic escape sequences
            'n' => {
                self.advance();
                Ok('\n')
            }
            't' => {
                self.advance();
                Ok('\t')
            }
            'r' => {
                self.advance();
                Ok('\r')
            }
            '\\' => {
                self.advance();
                Ok('\\')
            }
            '"' => {
                self.advance();
                Ok('"')
            }
            '\'' => {
                self.advance();
                Ok('\'')
            }
            // Hexadecimal escape: \xHH
            'x' => {
                self.advance();
                let mut value = 0u8;
                let mut count = 0;

                // Read up to 2 hex digits
                while count < 2 && !self.is_at_end() {
                    let c = self.current_char();
                    if let Some(digit) = c.to_digit(16) {
                        value = value * 16 + digit as u8;
                        self.advance();
                        count += 1;
                    } else {
                        break;
                    }
                }

                if count == 0 {
                    return Err("Invalid hexadecimal escape sequence".to_string());
                }

                Ok(value as char)
            }
            // Octal escape: \ooo (1-3 digits)
            '0'..='7' => {
                let mut value = 0u8;
                let mut count = 0;

                // Read up to 3 octal digits
                while count < 3 && !self.is_at_end() {
                    let c = self.current_char();
                    if c >= '0' && c <= '7' {
                        value = value * 8 + (c as u8 - b'0');
                        self.advance();
                        count += 1;
                    } else {
                        break;
                    }
                }

                Ok(value as char)
            }
            // For unrecognized escapes, just use the character
            _ => {
                let c = self.current_char();
                self.advance();
                Ok(c)
            }
        }
    }

    fn current_char(&self) -> char {
        self.input[self.position]
    }

    fn advance(&mut self) {
        if !self.is_at_end() && self.current_char() == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.position += 1;
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }
}
