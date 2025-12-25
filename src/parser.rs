use crate::ast::*;
use crate::lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Result<AstNode, String> {
        let mut functions = Vec::new();

        while !self.is_at_end() {
            if self.current() == &Token::Eof {
                break;
            }
            if self.is_struct_definition() {
                functions.push(self.parse_struct_definition()?);
            } else {
                functions.push(self.parse_function()?);
            }
        }

        Ok(AstNode::Program(functions))
    }

    fn parse_function(&mut self) -> Result<AstNode, String> {
        let return_type = self.parse_type()?;

        let name = match self.current() {
            Token::Identifier(s) => s.clone(),
            _ => return Err(format!("Expected function name, got {:?}", self.current())),
        };
        self.advance();

        self.expect(Token::OpenParen)?;
        let params = self.parse_parameters()?;
        self.expect(Token::CloseParen)?;

        let body = self.parse_block()?;

        Ok(AstNode::Function {
            name,
            return_type,
            params,
            body: Box::new(body),
        })
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let mut ty = match self.current() {
            Token::Unsigned => {
                self.advance();
                match self.current() {
                    Token::Char => {
                        self.advance();
                        Type::UChar
                    }
                    Token::Short => {
                        self.advance();
                        Type::UShort
                    }
                    Token::Long => {
                        self.advance();
                        if self.current() == &Token::Int {
                            self.advance();
                        }
                        Type::ULong
                    }
                    Token::Int => {
                        self.advance();
                        Type::UInt
                    }
                    _ => Type::UInt,
                }
            }
            Token::Short => {
                self.advance();
                if self.current() == &Token::Int {
                    self.advance();
                }
                Type::Short
            }
            Token::Long => {
                self.advance();
                if self.current() == &Token::Int {
                    self.advance();
                }
                Type::Long
            }
            Token::Int => {
                self.advance();
                Type::Int
            }
            Token::Char => {
                self.advance();
                Type::Char
            }
            Token::Struct => {
                self.advance();
                let name = match self.current() {
                    Token::Identifier(s) => s.clone(),
                    _ => return Err(format!("Expected struct name, got {:?}", self.current())),
                };
                self.advance();
                Type::Struct(name)
            }
            _ => return Err(format!("Expected type, got {:?}", self.current())),
        };

        while self.current() == &Token::Star {
            self.advance();
            ty = Type::Pointer(Box::new(ty));
        }

        Ok(ty)
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        let mut params = Vec::new();

        if self.current() == &Token::CloseParen {
            return Ok(params);
        }

        loop {
            let param_type = self.parse_type()?;

            let name = match self.current() {
                Token::Identifier(s) => s.clone(),
                _ => return Err(format!("Expected parameter name, got {:?}", self.current())),
            };
            self.advance();

            params.push(Parameter { name, param_type });

            if self.current() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        Ok(params)
    }

    fn parse_block(&mut self) -> Result<AstNode, String> {
        self.expect(Token::OpenBrace)?;

        let mut statements = Vec::new();
        while self.current() != &Token::CloseBrace {
            statements.push(self.parse_statement()?);
        }

        self.expect(Token::CloseBrace)?;
        Ok(AstNode::Block(statements))
    }

    fn parse_statement(&mut self) -> Result<AstNode, String> {
        match self.current() {
            Token::Return => self.parse_return(),
            Token::If => self.parse_if_statement(),
            Token::While => self.parse_while_loop(),
            Token::For => self.parse_for_loop(),
            Token::Unsigned | Token::Int | Token::Char | Token::Short | Token::Long | Token::Struct => {
                self.parse_var_decl()
            }
            Token::Identifier(_) => {
                let name = match self.current() {
                    Token::Identifier(s) => s.clone(),
                    _ => unreachable!(),
                };
                self.advance();

                if self.current() == &Token::Equals {
                    self.advance();
                    let value = self.parse_expression()?;
                    self.expect(Token::Semicolon)?;
                    Ok(AstNode::Assignment {
                        name,
                        value: Box::new(value),
                    })
                } else {
                    Err(format!(
                        "Expected '=' after identifier in statement, got {:?}",
                        self.current()
                    ))
                }
            }
            _ => Err(format!(
                "Unexpected token in statement: {:?}",
                self.current()
            )),
        }
    }

    fn parse_var_decl(&mut self) -> Result<AstNode, String> {
        let var_type = self.parse_type()?;

        let name = match self.current() {
            Token::Identifier(s) => s.clone(),
            _ => return Err(format!("Expected variable name, got {:?}", self.current())),
        };
        self.advance();

        let var_type = self.parse_array_type_suffix(var_type)?;

        let init = if self.current() == &Token::Equals {
            self.advance();
            if self.current() == &Token::OpenBrace {
                if matches!(var_type, Type::Struct(_)) {
                    Some(Box::new(self.parse_struct_initializer()?))
                } else {
                    Some(Box::new(self.parse_array_initializer()?))
                }
            } else {
                Some(Box::new(self.parse_expression()?))
            }
        } else {
            None
        };

        self.expect(Token::Semicolon)?;

        Ok(AstNode::VarDecl {
            name,
            var_type,
            init,
        })
    }

    fn parse_if_statement(&mut self) -> Result<AstNode, String> {
        self.expect(Token::If)?;
        self.expect(Token::OpenParen)?;
        let condition = self.parse_expression()?;
        self.expect(Token::CloseParen)?;

        let then_branch = if self.current() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        let else_branch = if self.current() == &Token::Else {
            self.advance();
            if self.current() == &Token::OpenBrace {
                Some(Box::new(self.parse_block()?))
            } else {
                Some(Box::new(self.parse_statement()?))
            }
        } else {
            None
        };

        Ok(AstNode::IfStatement {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    fn parse_while_loop(&mut self) -> Result<AstNode, String> {
        self.expect(Token::While)?;
        self.expect(Token::OpenParen)?;
        let condition = self.parse_expression()?;
        self.expect(Token::CloseParen)?;

        let body = if self.current() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        Ok(AstNode::WhileLoop {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_for_loop(&mut self) -> Result<AstNode, String> {
        self.expect(Token::For)?;
        self.expect(Token::OpenParen)?;

        let init = if self.current() == &Token::Semicolon {
            self.advance();
            None
        } else if self.current() == &Token::Int {
            let decl = self.parse_var_decl()?;
            Some(Box::new(decl))
        } else {
            return Err("For loop init must be a variable declaration or empty".to_string());
        };

        let condition = if self.current() == &Token::Semicolon {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };
        self.expect(Token::Semicolon)?;

        let increment = if self.current() == &Token::CloseParen {
            None
        } else {
            let name = match self.current() {
                Token::Identifier(s) => s.clone(),
                _ => {
                    return Err(format!(
                        "Expected identifier in for loop increment, got {:?}",
                        self.current()
                    ));
                }
            };
            self.advance();
            self.expect(Token::Equals)?;
            let value = self.parse_expression()?;
            Some(Box::new(AstNode::Assignment {
                name,
                value: Box::new(value),
            }))
        };

        self.expect(Token::CloseParen)?;

        let body = if self.current() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        Ok(AstNode::ForLoop {
            init,
            condition,
            increment,
            body: Box::new(body),
        })
    }

    fn parse_return(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Return)?;

        let expr = if self.current() == &Token::Semicolon {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.expect(Token::Semicolon)?;
        Ok(AstNode::Return(expr))
    }

    fn parse_expression(&mut self) -> Result<AstNode, String> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<AstNode, String> {
        let left = self.parse_logical_or()?;

        if self.current() == &Token::Equals {
            let name = match left {
                AstNode::Variable(name) => name,
                _ => {
                    return Err("Assignment target must be a variable".to_string());
                }
            };
            self.advance();
            let value = self.parse_assignment()?;
            Ok(AstNode::Assignment {
                name,
                value: Box::new(value),
            })
        } else {
            Ok(left)
        }
    }

    fn parse_logical_or(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_logical_and()?;

        while self.current() == &Token::LogicalOr {
            self.advance();
            let right = self.parse_logical_and()?;
            left = AstNode::BinaryOp {
                op: BinOp::LogicalOr,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_logical_and(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_equality()?;

        while self.current() == &Token::LogicalAnd {
            self.advance();
            let right = self.parse_equality()?;
            left = AstNode::BinaryOp {
                op: BinOp::LogicalAnd,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_comparison()?;

        while matches!(self.current(), Token::EqualEqual | Token::NotEqual) {
            let op = match self.current() {
                Token::EqualEqual => BinOp::EqualEqual,
                Token::NotEqual => BinOp::NotEqual,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_comparison()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_additive()?;

        while matches!(
            self.current(),
            Token::Less | Token::Greater | Token::LessEqual | Token::GreaterEqual
        ) {
            let op = match self.current() {
                Token::Less => BinOp::Less,
                Token::Greater => BinOp::Greater,
                Token::LessEqual => BinOp::LessEqual,
                Token::GreaterEqual => BinOp::GreaterEqual,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_additive()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_multiplicative()?;

        while matches!(self.current(), Token::Plus | Token::Minus) {
            let op = match self.current() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Subtract,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_unary()?;

        while matches!(self.current(), Token::Star | Token::Slash) {
            let op = match self.current() {
                Token::Star => BinOp::Multiply,
                Token::Slash => BinOp::Divide,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_unary()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<AstNode, String> {
        match self.current() {
            Token::LogicalNot => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::UnaryOp {
                    op: UnaryOp::LogicalNot,
                    operand: Box::new(operand),
                })
            }
            Token::Minus => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::UnaryOp {
                    op: UnaryOp::Negate,
                    operand: Box::new(operand),
                })
            }
            Token::Star => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::Dereference(Box::new(operand)))
            }
            Token::Ampersand => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::AddressOf(Box::new(operand)))
            }
            Token::Sizeof => self.parse_sizeof(),
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<AstNode, String> {
        let mut expr = match self.current() {
            Token::IntLiteral(n) => {
                let val = *n;
                self.advance();
                AstNode::IntLiteral(val)
            }
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                AstNode::Variable(name)
            }
            Token::OpenParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::CloseParen)?;
                expr
            }
            _ => return Err(format!("Expected expression, got {:?}", self.current())),
        };

        loop {
            match self.current() {
                Token::OpenParen => {
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(Token::CloseParen)?;
                    if let AstNode::Variable(name) = expr {
                        expr = AstNode::FunctionCall { name, args };
                    } else {
                        return Err("Function call target must be an identifier".to_string());
                    }
                }
                Token::OpenBracket => {
                    self.advance();
                    let index = self.parse_expression()?;
                    self.expect(Token::CloseBracket)?;
                    expr = AstNode::ArrayIndex {
                        array: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                Token::Dot | Token::Arrow => {
                    let through_pointer = self.current() == &Token::Arrow;
                    self.advance();
                    let member = match self.current() {
                        Token::Identifier(s) => s.clone(),
                        _ => return Err(format!("Expected member name, got {:?}", self.current())),
                    };
                    self.advance();
                    expr = AstNode::MemberAccess {
                        base: Box::new(expr),
                        member,
                        through_pointer,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_arguments(&mut self) -> Result<Vec<AstNode>, String> {
        let mut args = Vec::new();

        if self.current() == &Token::CloseParen {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expression()?);

            if self.current() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        Ok(args)
    }

    fn current(&self) -> &Token {
        &self.tokens[self.position]
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.position += 1;
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if self.current() == &expected {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, got {:?}", expected, self.current()))
        }
    }

    fn parse_array_type_suffix(&mut self, base: Type) -> Result<Type, String> {
        let mut ty = base;
        while self.current() == &Token::OpenBracket {
            self.advance();
            let len = match self.current() {
                Token::IntLiteral(n) if *n >= 0 => *n as usize,
                _ => return Err("Array length must be a non-negative integer literal".to_string()),
            };
            self.advance();
            self.expect(Token::CloseBracket)?;
            ty = Type::Array(Box::new(ty), len);
        }
        Ok(ty)
    }

    fn parse_array_initializer(&mut self) -> Result<AstNode, String> {
        self.expect(Token::OpenBrace)?;
        let mut values = Vec::new();

        if self.current() == &Token::CloseBrace {
            self.advance();
            return Ok(AstNode::ArrayInit(values));
        }

        loop {
            values.push(self.parse_expression()?);
            if self.current() == &Token::Comma {
                self.advance();
                if self.current() == &Token::CloseBrace {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect(Token::CloseBrace)?;
        Ok(AstNode::ArrayInit(values))
    }

    fn parse_struct_definition(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Struct)?;
        let name = match self.current() {
            Token::Identifier(s) => s.clone(),
            _ => return Err(format!("Expected struct name, got {:?}", self.current())),
        };
        self.advance();
        self.expect(Token::OpenBrace)?;

        let mut fields = Vec::new();
        while self.current() != &Token::CloseBrace {
            let field_type = self.parse_type()?;
            let field_name = match self.current() {
                Token::Identifier(s) => s.clone(),
                _ => return Err(format!("Expected field name, got {:?}", self.current())),
            };
            self.advance();
            let field_type = self.parse_array_type_suffix(field_type)?;
            self.expect(Token::Semicolon)?;
            fields.push(StructField {
                name: field_name,
                field_type,
            });
        }

        self.expect(Token::CloseBrace)?;
        self.expect(Token::Semicolon)?;

        Ok(AstNode::StructDef { name, fields })
    }

    fn parse_struct_initializer(&mut self) -> Result<AstNode, String> {
        self.expect(Token::OpenBrace)?;
        let mut values = Vec::new();

        if self.current() == &Token::CloseBrace {
            self.advance();
            return Ok(AstNode::StructInit(values));
        }

        loop {
            values.push(self.parse_expression()?);
            if self.current() == &Token::Comma {
                self.advance();
                if self.current() == &Token::CloseBrace {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect(Token::CloseBrace)?;
        Ok(AstNode::StructInit(values))
    }

    fn parse_sizeof(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Sizeof)?;

        if self.current() == &Token::OpenParen {
            if self.is_type_start(self.peek(1)) {
                self.advance();
                let ty = self.parse_type()?;
                let ty = self.parse_array_type_suffix(ty)?;
                self.expect(Token::CloseParen)?;
                Ok(AstNode::SizeOfType(ty))
            } else {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::CloseParen)?;
                Ok(AstNode::SizeOfExpr(Box::new(expr)))
            }
        } else {
            let expr = self.parse_unary()?;
            Ok(AstNode::SizeOfExpr(Box::new(expr)))
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len() || self.current() == &Token::Eof
    }

    fn is_struct_definition(&self) -> bool {
        matches!(self.current(), Token::Struct)
            && matches!(self.peek(1), Some(Token::Identifier(_)))
            && matches!(self.peek(2), Some(Token::OpenBrace))
    }

    fn is_type_start(&self, token: Option<&Token>) -> bool {
        matches!(
            token,
            Some(Token::Unsigned)
                | Some(Token::Int)
                | Some(Token::Char)
                | Some(Token::Struct)
                | Some(Token::Short)
                | Some(Token::Long)
        )
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset)
    }
}
