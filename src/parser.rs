use crate::ast::*;
use crate::lexer::{LocatedToken, SourceLocation, Token};

pub struct Parser {
    tokens: Vec<LocatedToken>,
    position: usize,
    type_aliases: std::collections::HashMap<String, Type>,
}

impl Parser {
    pub fn new(tokens: Vec<LocatedToken>) -> Self {
        Parser {
            tokens,
            position: 0,
            type_aliases: std::collections::HashMap::new(),
        }
    }

    /// Get the current token
    fn current_token(&self) -> &Token {
        &self.tokens[self.position].token
    }

    /// Get the current location
    fn current_location(&self) -> SourceLocation {
        self.tokens[self.position].location
    }

    pub fn parse(&mut self) -> Result<AstNode, String> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            if self.current_token() == &Token::Eof {
                break;
            }
            if self.is_struct_definition() {
                items.push(self.parse_struct_definition()?);
            } else if self.is_union_definition() {
                items.push(self.parse_union_definition()?);
            } else if self.is_enum_definition() {
                items.push(self.parse_enum_definition()?);
            } else {
                // Could be a function or global variable
                items.push(self.parse_declaration()?);
            }
        }

        Ok(AstNode::Program(items))
    }

    fn parse_declaration(&mut self) -> Result<AstNode, String> {
        // Check for typedef first
        if matches!(self.current_token(), Token::Typedef) {
            return self.parse_typedef();
        }

        // Check for storage class specifiers and type qualifiers
        let mut is_extern = false;
        let mut is_static = false;
        let mut is_const = false;
        let mut is_volatile = false;

        // Parse storage class and qualifiers (can appear in any order)
        loop {
            match self.current_token() {
                Token::Extern if !is_extern => {
                    self.advance();
                    is_extern = true;
                }
                Token::Static if !is_static => {
                    self.advance();
                    is_static = true;
                }
                Token::Const if !is_const => {
                    self.advance();
                    is_const = true;
                }
                Token::Volatile if !is_volatile => {
                    self.advance();
                    is_volatile = true;
                }
                _ => break,
            }
        }

        let var_type = self.parse_type()?;

        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => {
                return Err(format!(
                    "Expected identifier, got {:?} at {}",
                    self.current_token(),
                    self.current_location()
                ))
            }
        };
        self.advance();

        // Check what follows to determine if it's a function or variable
        match self.current_token() {
            Token::OpenParen => {
                // It's a function (extern/static don't affect function declarations in our simple compiler)
                self.parse_function_rest(var_type, name)
            }
            Token::Semicolon | Token::Equals | Token::OpenBracket => {
                // It's a global variable
                self.parse_global_variable_with_storage(var_type, name, is_extern, is_static, is_const, is_volatile)
            }
            _ => Err(format!(
                "Expected '(', ';', '=', or '[' after identifier, got {:?}",
                self.current_token()
            )),
        }
    }

    fn parse_function(&mut self) -> Result<AstNode, String> {
        let return_type = self.parse_type()?;

        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => {
                return Err(format!(
                    "Expected function name, got {:?} at {}",
                    self.current_token(),
                    self.current_location()
                ))
            }
        };
        self.advance();

        self.parse_function_rest(return_type, name)
    }

    fn parse_function_rest(
        &mut self,
        return_type: Type,
        name: String,
    ) -> Result<AstNode, String> {
        self.expect(Token::OpenParen)?;
        let params = self.parse_parameters()?;
        self.expect(Token::CloseParen)?;

        // Check if this is a forward declaration (semicolon) or definition (body)
        let body = if self.current_token() == &Token::Semicolon {
            // Forward declaration
            self.advance();
            None
        } else {
            // Function definition
            Some(Box::new(self.parse_block()?))
        };

        Ok(AstNode::Function {
            name,
            return_type,
            params,
            body,
        })
    }

    fn parse_global_variable(
        &mut self,
        mut var_type: Type,
        name: String,
    ) -> Result<AstNode, String> {
        self.parse_global_variable_with_storage(var_type, name, false, false, false, false)
    }

    fn parse_global_variable_with_storage(
        &mut self,
        mut var_type: Type,
        name: String,
        is_extern: bool,
        is_static: bool,
        is_const: bool,
        is_volatile: bool,
    ) -> Result<AstNode, String> {
        // Handle array type suffix if present
        var_type = self.parse_array_type_suffix(var_type)?;

        // Parse initializer if present
        let init = if self.current_token() == &Token::Equals {
            self.advance();
            if self.current_token() == &Token::OpenBrace {
                if matches!(var_type, Type::Struct(_) | Type::Union(_)) {
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
            is_extern,
            is_static,
            is_const,
            is_volatile,
        })
    }

    fn parse_typedef(&mut self) -> Result<AstNode, String> {
        // Consume 'typedef' token
        self.expect(Token::Typedef)?;

        // Parse the target type
        let target_type = self.parse_type()?;

        // Parse the alias name
        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => return Err(format!("Expected identifier after typedef, got {:?}", self.current_token())),
        };
        self.advance();

        // Expect semicolon
        self.expect(Token::Semicolon)?;

        // Store the type alias
        self.type_aliases.insert(name.clone(), target_type.clone());

        Ok(AstNode::TypedefDef {
            name,
            target_type,
        })
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let mut ty = match self.current_token() {
            Token::Unsigned => {
                self.advance();
                match self.current_token() {
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
                        if self.current_token() == &Token::Int {
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
                if self.current_token() == &Token::Int {
                    self.advance();
                }
                Type::Short
            }
            Token::Long => {
                self.advance();
                if self.current_token() == &Token::Int {
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
            Token::Void => {
                self.advance();
                Type::Void
            }
            Token::Struct => {
                self.advance();
                let name = match self.current_token() {
                    Token::Identifier(s) => s.clone(),
                    _ => return Err(format!("Expected struct name, got {:?}", self.current_token())),
                };
                self.advance();
                Type::Struct(name)
            }
            Token::Union => {
                self.advance();
                let name = match self.current_token() {
                    Token::Identifier(s) => s.clone(),
                    _ => return Err(format!("Expected union name, got {:?}", self.current_token())),
                };
                self.advance();
                Type::Union(name)
            }
            Token::Enum => {
                self.advance();
                let name = match self.current_token() {
                    Token::Identifier(s) => s.clone(),
                    _ => return Err(format!("Expected enum name, got {:?}", self.current_token())),
                };
                self.advance();
                Type::Enum(name)
            }
            Token::Identifier(name) => {
                // Check if this is a typedef'd type
                if let Some(aliased_type) = self.type_aliases.get(name).cloned() {
                    self.advance();
                    aliased_type
                } else {
                    return Err(format!("Unknown type: {}", name));
                }
            }
            _ => return Err(format!("Expected type, got {:?}", self.current_token())),
        };

        while self.current_token() == &Token::Star {
            self.advance();
            ty = Type::Pointer(Box::new(ty));
        }

        Ok(ty)
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        let mut params = Vec::new();

        if self.current_token() == &Token::CloseParen {
            return Ok(params);
        }

        loop {
            let param_type = self.parse_type()?;

            let name = match self.current_token() {
                Token::Identifier(s) => s.clone(),
                _ => return Err(format!("Expected parameter name, got {:?}", self.current_token())),
            };
            self.advance();

            params.push(Parameter { name, param_type });

            if self.current_token() == &Token::Comma {
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
        while self.current_token() != &Token::CloseBrace {
            statements.push(self.parse_statement()?);
        }

        self.expect(Token::CloseBrace)?;
        Ok(AstNode::Block(statements))
    }

    fn parse_statement(&mut self) -> Result<AstNode, String> {
        match self.current_token() {
            Token::Return => self.parse_return(),
            Token::If => self.parse_if_statement(),
            Token::While => self.parse_while_loop(),
            Token::Do => self.parse_do_while_loop(),
            Token::For => self.parse_for_loop(),
            Token::Break => self.parse_break(),
            Token::Continue => self.parse_continue(),
            Token::Goto => self.parse_goto(),
            Token::Unsigned
            | Token::Int
            | Token::Char
            | Token::Short
            | Token::Long
            | Token::Struct
            | Token::Union
            | Token::Enum
            | Token::Void
            | Token::Static
            | Token::Const
            | Token::Volatile => self.parse_var_decl(),
            Token::Identifier(name) if self.type_aliases.contains_key(name) => {
                // This is a typedef'd type, parse as variable declaration
                self.parse_var_decl()
            }
            Token::Identifier(name) if matches!(self.peek(1), Some(&Token::Colon)) => {
                // This is a label
                let label_name = name.clone();
                self.advance(); // consume identifier
                self.advance(); // consume colon
                Ok(AstNode::Label(label_name))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(expr)
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<AstNode, String> {
        // Check for storage class and qualifiers
        let mut is_static = false;
        let mut is_const = false;
        let mut is_volatile = false;

        // Parse storage class and qualifiers (can appear in any order)
        loop {
            match self.current_token() {
                Token::Static if !is_static => {
                    self.advance();
                    is_static = true;
                }
                Token::Const if !is_const => {
                    self.advance();
                    is_const = true;
                }
                Token::Volatile if !is_volatile => {
                    self.advance();
                    is_volatile = true;
                }
                _ => break,
            }
        }

        let var_type = self.parse_type()?;

        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => return Err(format!("Expected variable name, got {:?}", self.current_token())),
        };
        self.advance();

        let var_type = self.parse_array_type_suffix(var_type)?;

        let init = if self.current_token() == &Token::Equals {
            self.advance();
            if self.current_token() == &Token::OpenBrace {
                if matches!(var_type, Type::Struct(_) | Type::Union(_)) {
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
            is_extern: false,
            is_static,
            is_const,
            is_volatile,
        })
    }

    fn parse_if_statement(&mut self) -> Result<AstNode, String> {
        self.expect(Token::If)?;
        self.expect(Token::OpenParen)?;
        let condition = self.parse_expression()?;
        self.expect(Token::CloseParen)?;

        let then_branch = if self.current_token() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        let else_branch = if self.current_token() == &Token::Else {
            self.advance();
            if self.current_token() == &Token::OpenBrace {
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

        let body = if self.current_token() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        Ok(AstNode::WhileLoop {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_do_while_loop(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Do)?;

        let body = if self.current_token() == &Token::OpenBrace {
            self.parse_block()?
        } else {
            self.parse_statement()?
        };

        self.expect(Token::While)?;
        self.expect(Token::OpenParen)?;
        let condition = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        self.expect(Token::Semicolon)?;

        Ok(AstNode::DoWhileLoop {
            body: Box::new(body),
            condition: Box::new(condition),
        })
    }

    fn parse_for_loop(&mut self) -> Result<AstNode, String> {
        self.expect(Token::For)?;
        self.expect(Token::OpenParen)?;

        let init = if self.current_token() == &Token::Semicolon {
            self.advance();
            None
        } else if self.current_token() == &Token::Int {
            let decl = self.parse_var_decl()?;
            Some(Box::new(decl))
        } else {
            return Err("For loop init must be a variable declaration or empty".to_string());
        };

        let condition = if self.current_token() == &Token::Semicolon {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };
        self.expect(Token::Semicolon)?;

        let increment = if self.current_token() == &Token::CloseParen {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.expect(Token::CloseParen)?;

        let body = if self.current_token() == &Token::OpenBrace {
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

        let expr = if self.current_token() == &Token::Semicolon {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.expect(Token::Semicolon)?;
        Ok(AstNode::Return(expr))
    }

    fn parse_break(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Break)?;
        self.expect(Token::Semicolon)?;
        Ok(AstNode::Break)
    }

    fn parse_continue(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Continue)?;
        self.expect(Token::Semicolon)?;
        Ok(AstNode::Continue)
    }

    fn parse_goto(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Goto)?;
        let label = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => return Err(format!("Expected label name after goto, got {:?}", self.current_token())),
        };
        self.advance();
        self.expect(Token::Semicolon)?;
        Ok(AstNode::Goto(label))
    }

    fn parse_expression(&mut self) -> Result<AstNode, String> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<AstNode, String> {
        let left = self.parse_ternary()?;

        match self.current_token() {
            Token::Equals
            | Token::PlusEquals
            | Token::MinusEquals
            | Token::StarEquals
            | Token::SlashEquals
            | Token::PercentEquals
            | Token::AmpersandEquals
            | Token::PipeEquals
            | Token::CaretEquals
            | Token::LessLessEquals
            | Token::GreaterGreaterEquals => {
                // left is now the lvalue (could be Variable, MemberAccess, ArrayIndex, Dereference)
                let op_token = self.current_token().clone();
                self.advance();
                let right_value = self.parse_assignment()?;

                let value = match op_token {
                    Token::Equals => right_value,
                    Token::PlusEquals => AstNode::BinaryOp {
                        op: BinOp::Add,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::MinusEquals => AstNode::BinaryOp {
                        op: BinOp::Subtract,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::StarEquals => AstNode::BinaryOp {
                        op: BinOp::Multiply,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::SlashEquals => AstNode::BinaryOp {
                        op: BinOp::Divide,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::PercentEquals => AstNode::BinaryOp {
                        op: BinOp::Modulo,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::AmpersandEquals => AstNode::BinaryOp {
                        op: BinOp::BitAnd,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::PipeEquals => AstNode::BinaryOp {
                        op: BinOp::BitOr,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::CaretEquals => AstNode::BinaryOp {
                        op: BinOp::BitXor,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::LessLessEquals => AstNode::BinaryOp {
                        op: BinOp::ShiftLeft,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    Token::GreaterGreaterEquals => AstNode::BinaryOp {
                        op: BinOp::ShiftRight,
                        left: Box::new(left.clone()),
                        right: Box::new(right_value),
                    },
                    _ => unreachable!(),
                };

                Ok(AstNode::Assignment {
                    target: Box::new(left),
                    value: Box::new(value),
                })
            }
            _ => Ok(left),
        }
    }

    fn parse_ternary(&mut self) -> Result<AstNode, String> {
        let condition = self.parse_logical_or()?;

        if self.current_token() == &Token::Question {
            self.advance();
            let true_expr = self.parse_expression()?;
            self.expect(Token::Colon)?;
            let false_expr = self.parse_ternary()?;

            Ok(AstNode::TernaryOp {
                condition: Box::new(condition),
                true_expr: Box::new(true_expr),
                false_expr: Box::new(false_expr),
            })
        } else {
            Ok(condition)
        }
    }

    fn parse_logical_or(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_logical_and()?;

        while self.current_token() == &Token::LogicalOr {
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
        let mut left = self.parse_bitwise_or()?;

        while self.current_token() == &Token::LogicalAnd {
            self.advance();
            let right = self.parse_bitwise_or()?;
            left = AstNode::BinaryOp {
                op: BinOp::LogicalAnd,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_bitwise_or(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_bitwise_xor()?;

        while self.current_token() == &Token::Pipe {
            self.advance();
            let right = self.parse_bitwise_xor()?;
            left = AstNode::BinaryOp {
                op: BinOp::BitOr,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_bitwise_xor(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_bitwise_and()?;

        while self.current_token() == &Token::Caret {
            self.advance();
            let right = self.parse_bitwise_and()?;
            left = AstNode::BinaryOp {
                op: BinOp::BitXor,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_bitwise_and(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_equality()?;

        while self.current_token() == &Token::Ampersand {
            self.advance();
            let right = self.parse_equality()?;
            left = AstNode::BinaryOp {
                op: BinOp::BitAnd,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_comparison()?;

        while matches!(self.current_token(), Token::EqualEqual | Token::NotEqual) {
            let op = match self.current_token() {
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
        let mut left = self.parse_shift()?;

        while matches!(
            self.current_token(),
            Token::Less | Token::Greater | Token::LessEqual | Token::GreaterEqual
        ) {
            let op = match self.current_token() {
                Token::Less => BinOp::Less,
                Token::Greater => BinOp::Greater,
                Token::LessEqual => BinOp::LessEqual,
                Token::GreaterEqual => BinOp::GreaterEqual,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_shift()?;
            left = AstNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_shift(&mut self) -> Result<AstNode, String> {
        let mut left = self.parse_additive()?;

        while matches!(self.current_token(), Token::LessLess | Token::GreaterGreater) {
            let op = match self.current_token() {
                Token::LessLess => BinOp::ShiftLeft,
                Token::GreaterGreater => BinOp::ShiftRight,
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

        while matches!(self.current_token(), Token::Plus | Token::Minus) {
            let op = match self.current_token() {
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

        while matches!(self.current_token(), Token::Star | Token::Slash | Token::Percent) {
            let op = match self.current_token() {
                Token::Star => BinOp::Multiply,
                Token::Slash => BinOp::Divide,
                Token::Percent => BinOp::Modulo,
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
        match self.current_token() {
            Token::PlusPlus => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::PrefixIncrement(Box::new(operand)))
            }
            Token::MinusMinus => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::PrefixDecrement(Box::new(operand)))
            }
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
            Token::Tilde => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(AstNode::UnaryOp {
                    op: UnaryOp::BitNot,
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
            Token::OpenParen => {
                // Check if this is a cast expression: (type)expr
                if self.is_type_start(self.peek(1)) {
                    self.advance(); // consume (
                    let target_type = self.parse_type()?;
                    self.expect(Token::CloseParen)?;
                    let expr = self.parse_unary()?;
                    Ok(AstNode::Cast {
                        target_type,
                        expr: Box::new(expr),
                    })
                } else {
                    self.parse_primary()
                }
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<AstNode, String> {
        let mut expr = match self.current_token() {
            Token::IntLiteral(n) => {
                let val = *n;
                self.advance();
                AstNode::IntLiteral(val)
            }
            Token::CharLiteral(c) => {
                let val = *c;
                self.advance();
                AstNode::CharLiteral(val)
            }
            Token::StringLiteral(s) => {
                let val = s.clone();
                self.advance();
                AstNode::StringLiteral(val)
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
            _ => return Err(format!("Expected expression, got {:?}", self.current_token())),
        };

        loop {
            match self.current_token() {
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
                    let through_pointer = self.current_token() == &Token::Arrow;
                    self.advance();
                    let member = match self.current_token() {
                        Token::Identifier(s) => s.clone(),
                        _ => return Err(format!("Expected member name, got {:?}", self.current_token())),
                    };
                    self.advance();
                    expr = AstNode::MemberAccess {
                        base: Box::new(expr),
                        member,
                        through_pointer,
                    };
                }
                Token::PlusPlus => {
                    self.advance();
                    expr = AstNode::PostfixIncrement(Box::new(expr));
                }
                Token::MinusMinus => {
                    self.advance();
                    expr = AstNode::PostfixDecrement(Box::new(expr));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_arguments(&mut self) -> Result<Vec<AstNode>, String> {
        let mut args = Vec::new();

        if self.current_token() == &Token::CloseParen {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expression()?);

            if self.current_token() == &Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        Ok(args)
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.position += 1;
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if self.current_token() == &expected {
            self.advance();
            Ok(())
        } else {
            Err(format!(
                "Expected {:?}, got {:?} at {}",
                expected,
                self.current_token(),
                self.current_location()
            ))
        }
    }

    fn parse_array_type_suffix(&mut self, base: Type) -> Result<Type, String> {
        // Collect all array dimensions first
        let mut dimensions = Vec::new();
        while self.current_token() == &Token::OpenBracket {
            self.advance();
            let len = match self.current_token() {
                Token::IntLiteral(n) if *n >= 0 => *n as usize,
                _ => return Err("Array length must be a non-negative integer literal".to_string()),
            };
            self.advance();
            self.expect(Token::CloseBracket)?;
            dimensions.push(len);
        }

        // Build array type from innermost to outermost (right to left)
        // For int x[2][3], dimensions = [2, 3]
        // We want Array(Array(Int, 3), 2)
        let mut ty = base;
        for &len in dimensions.iter().rev() {
            ty = Type::Array(Box::new(ty), len);
        }
        Ok(ty)
    }

    fn parse_array_initializer(&mut self) -> Result<AstNode, String> {
        self.expect(Token::OpenBrace)?;
        let mut values = Vec::new();

        if self.current_token() == &Token::CloseBrace {
            self.advance();
            return Ok(AstNode::ArrayInit(values));
        }

        loop {
            // Check if this is a nested initializer (for multi-dimensional arrays)
            if self.current_token() == &Token::OpenBrace {
                values.push(self.parse_array_initializer()?);
            } else {
                values.push(self.parse_expression()?);
            }

            if self.current_token() == &Token::Comma {
                self.advance();
                if self.current_token() == &Token::CloseBrace {
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
        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => return Err(format!("Expected struct name, got {:?}", self.current_token())),
        };
        self.advance();
        self.expect(Token::OpenBrace)?;

        let mut fields = Vec::new();
        while self.current_token() != &Token::CloseBrace {
            let field_type = self.parse_type()?;
            let field_name = match self.current_token() {
                Token::Identifier(s) => s.clone(),
                _ => return Err(format!("Expected field name, got {:?}", self.current_token())),
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
        let mut fields = Vec::new();

        if self.current_token() == &Token::CloseBrace {
            self.advance();
            return Ok(AstNode::StructInit(fields));
        }

        loop {
            // Check if this is a designated initializer (.field = value)
            let field_name = if self.current_token() == &Token::Dot {
                self.advance(); // consume '.'
                match self.current_token() {
                    Token::Identifier(name) => {
                        let field = name.clone();
                        self.advance(); // consume field name
                        self.expect(Token::Equals)?;
                        Some(field)
                    }
                    _ => return Err(format!("Expected field name after '.', got {:?}", self.current_token())),
                }
            } else {
                None // Positional initializer
            };

            // Parse the value - check if it's a nested struct/union initializer
            let value = if self.current_token() == &Token::OpenBrace {
                // Nested struct/union initialization
                self.parse_struct_initializer()?
            } else {
                self.parse_expression()?
            };
            fields.push(StructInitField { field_name, value });

            if self.current_token() == &Token::Comma {
                self.advance();
                if self.current_token() == &Token::CloseBrace {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect(Token::CloseBrace)?;
        Ok(AstNode::StructInit(fields))
    }

    fn parse_union_definition(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Union)?;
        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => return Err(format!("Expected union name, got {:?}", self.current_token())),
        };
        self.advance();
        self.expect(Token::OpenBrace)?;

        let mut fields = Vec::new();
        while self.current_token() != &Token::CloseBrace {
            let field_type = self.parse_type()?;
            let field_name = match self.current_token() {
                Token::Identifier(s) => s.clone(),
                _ => return Err(format!("Expected field name, got {:?}", self.current_token())),
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

        Ok(AstNode::UnionDef { name, fields })
    }

    fn parse_enum_definition(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Enum)?;
        let name = match self.current_token() {
            Token::Identifier(s) => s.clone(),
            _ => return Err(format!("Expected enum name, got {:?}", self.current_token())),
        };
        self.advance();
        self.expect(Token::OpenBrace)?;

        let mut enumerators = Vec::new();
        let mut next_value = 0i64;

        while self.current_token() != &Token::CloseBrace {
            let enumerator_name = match self.current_token() {
                Token::Identifier(s) => s.clone(),
                _ => {
                    return Err(format!(
                        "Expected enumerator name, got {:?}",
                        self.current_token()
                    ));
                }
            };
            self.advance();

            let value = if self.current_token() == &Token::Equals {
                self.advance();
                match self.current_token() {
                    Token::IntLiteral(n) => {
                        let val = *n;
                        self.advance();
                        next_value = val + 1;
                        Some(val)
                    }
                    _ => return Err("Enumerator value must be an integer literal".to_string()),
                }
            } else {
                let val = next_value;
                next_value += 1;
                Some(val)
            };

            enumerators.push(Enumerator {
                name: enumerator_name,
                value,
            });

            if self.current_token() == &Token::Comma {
                self.advance();
                if self.current_token() == &Token::CloseBrace {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect(Token::CloseBrace)?;
        self.expect(Token::Semicolon)?;

        Ok(AstNode::EnumDef { name, enumerators })
    }

    fn parse_sizeof(&mut self) -> Result<AstNode, String> {
        self.expect(Token::Sizeof)?;

        if self.current_token() == &Token::OpenParen {
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
        self.position >= self.tokens.len() || self.current_token() == &Token::Eof
    }

    fn is_struct_definition(&self) -> bool {
        matches!(self.current_token(), Token::Struct)
            && matches!(self.peek(1), Some(Token::Identifier(_)))
            && matches!(self.peek(2), Some(Token::OpenBrace))
    }

    fn is_union_definition(&self) -> bool {
        matches!(self.current_token(), Token::Union)
            && matches!(self.peek(1), Some(Token::Identifier(_)))
            && matches!(self.peek(2), Some(Token::OpenBrace))
    }

    fn is_enum_definition(&self) -> bool {
        matches!(self.current_token(), Token::Enum)
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
                | Some(Token::Union)
                | Some(Token::Enum)
                | Some(Token::Short)
                | Some(Token::Long)
                | Some(Token::Void)
        )
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset).map(|lt| &lt.token)
    }
}
