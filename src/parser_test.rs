#[cfg(test)]
mod parser_tests {
    use crate::ast::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_parse_simple_function() {
        let mut lexer = Lexer::new("int main() { return 42; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => {
                assert_eq!(funcs.len(), 1);
                match &funcs[0] {
                    AstNode::Function { name, .. } => {
                        assert_eq!(name, "main");
                    }
                    _ => panic!("Expected function"),
                }
            }
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_arithmetic_expression() {
        let mut lexer = Lexer::new("int main() { return 2 + 3; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => match &stmts[0] {
                        AstNode::Return(Some(expr)) => match expr.as_ref() {
                            AstNode::BinaryOp { op, .. } => {
                                assert_eq!(*op, BinOp::Add);
                            }
                            _ => panic!("Expected binary op"),
                        },
                        _ => panic!("Expected return statement"),
                    },
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_operator_precedence() {
        let mut lexer = Lexer::new("int main() { return 2 + 3 * 4; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => match &stmts[0] {
                        AstNode::Return(Some(expr)) => match expr.as_ref() {
                            AstNode::BinaryOp {
                                op: BinOp::Add,
                                left,
                                right,
                            } => {
                                match left.as_ref() {
                                    AstNode::IntLiteral(2) => {}
                                    _ => panic!("Expected left to be 2"),
                                }
                                match right.as_ref() {
                                    AstNode::BinaryOp {
                                        op: BinOp::Multiply,
                                        ..
                                    } => {}
                                    _ => panic!("Expected right to be multiplication"),
                                }
                            }
                            _ => panic!("Expected addition at top level"),
                        },
                        _ => panic!("Expected return statement"),
                    },
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parentheses() {
        let mut lexer = Lexer::new("int main() { return (2 + 3) * 4; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => match &stmts[0] {
                        AstNode::Return(Some(expr)) => match expr.as_ref() {
                            AstNode::BinaryOp {
                                op: BinOp::Multiply,
                                left,
                                right,
                            } => {
                                match left.as_ref() {
                                    AstNode::BinaryOp { op: BinOp::Add, .. } => {}
                                    _ => panic!("Expected left to be addition"),
                                }
                                match right.as_ref() {
                                    AstNode::IntLiteral(4) => {}
                                    _ => panic!("Expected right to be 4"),
                                }
                            }
                            _ => panic!("Expected multiplication at top level"),
                        },
                        _ => panic!("Expected return statement"),
                    },
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_variable_declaration() {
        let mut lexer = Lexer::new("int main() { int x = 5; return x; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => {
                        assert_eq!(stmts.len(), 2);
                        match &stmts[0] {
                            AstNode::VarDecl { name, .. } => {
                                assert_eq!(name, "x");
                            }
                            _ => panic!("Expected var decl"),
                        }
                    }
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_function_call() {
        let mut lexer = Lexer::new("int main() { return add(2, 3); }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => match &stmts[0] {
                        AstNode::Return(Some(expr)) => match expr.as_ref() {
                            AstNode::FunctionCall { name, args } => {
                                assert_eq!(name, "add");
                                assert_eq!(args.len(), 2);
                            }
                            _ => panic!("Expected function call"),
                        },
                        _ => panic!("Expected return"),
                    },
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_function_with_parameters() {
        let mut lexer = Lexer::new("int add(int a, int b) { return a + b; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { name, params, .. } => {
                    assert_eq!(name, "add");
                    assert_eq!(params.len(), 2);
                    assert_eq!(params[0].name, "a");
                    assert_eq!(params[1].name, "b");
                }
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_pointer_and_address_of() {
        let mut lexer = Lexer::new("int main() { int x = 1; int* p = &x; return *p; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => {
                        match &stmts[1] {
                            AstNode::VarDecl {
                                name,
                                var_type,
                                init,
                            } => {
                                assert_eq!(name, "p");
                                assert_eq!(var_type, &Type::Pointer(Box::new(Type::Int)));
                                match init.as_deref() {
                                    Some(AstNode::AddressOf(inner)) => match inner.as_ref() {
                                        AstNode::Variable(var) => assert_eq!(var, "x"),
                                        _ => panic!("Expected address-of variable"),
                                    },
                                    _ => panic!("Expected address-of initializer"),
                                }
                            }
                            _ => panic!("Expected pointer var decl"),
                        }

                        match &stmts[2] {
                            AstNode::Return(Some(expr)) => match expr.as_ref() {
                                AstNode::Dereference(inner) => match inner.as_ref() {
                                    AstNode::Variable(var) => assert_eq!(var, "p"),
                                    _ => panic!("Expected dereference of variable"),
                                },
                                _ => panic!("Expected dereference in return"),
                            },
                            _ => panic!("Expected return statement"),
                        }
                    }
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_array_declaration_and_index() {
        let mut lexer = Lexer::new("int main() { int a[3] = {1, 2, 3}; return a[1]; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => {
                        match &stmts[0] {
                            AstNode::VarDecl {
                                name,
                                var_type,
                                init,
                            } => {
                                assert_eq!(name, "a");
                                assert_eq!(var_type, &Type::Array(Box::new(Type::Int), 3));
                                match init.as_deref() {
                                    Some(AstNode::ArrayInit(values)) => {
                                        assert_eq!(values.len(), 3);
                                    }
                                    _ => panic!("Expected array initializer"),
                                }
                            }
                            _ => panic!("Expected array var decl"),
                        }

                        match &stmts[1] {
                            AstNode::Return(Some(expr)) => match expr.as_ref() {
                                AstNode::ArrayIndex { array, index } => {
                                    match array.as_ref() {
                                        AstNode::Variable(var) => assert_eq!(var, "a"),
                                        _ => panic!("Expected array variable"),
                                    }
                                    match index.as_ref() {
                                        AstNode::IntLiteral(1) => {}
                                        _ => panic!("Expected index literal"),
                                    }
                                }
                                _ => panic!("Expected array index in return"),
                            },
                            _ => panic!("Expected return statement"),
                        }
                    }
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_struct_definition_and_member_access() {
        let mut lexer = Lexer::new(
            "struct Point { int x; int y; }; int main() { struct Point p = {1, 2}; return p.x; }",
        );
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(nodes) => {
                match &nodes[0] {
                    AstNode::StructDef { name, fields } => {
                        assert_eq!(name, "Point");
                        assert_eq!(fields.len(), 2);
                        assert_eq!(fields[0].name, "x");
                    }
                    _ => panic!("Expected struct definition"),
                }
                match &nodes[1] {
                    AstNode::Function { body, .. } => match body.as_ref() {
                        AstNode::Block(stmts) => {
                            match &stmts[0] {
                                AstNode::VarDecl {
                                    name,
                                    var_type,
                                    init,
                                } => {
                                    assert_eq!(name, "p");
                                    assert_eq!(var_type, &Type::Struct("Point".to_string()));
                                    match init.as_deref() {
                                        Some(AstNode::StructInit(values)) => {
                                            assert_eq!(values.len(), 2);
                                        }
                                        _ => panic!("Expected struct initializer"),
                                    }
                                }
                                _ => panic!("Expected struct var decl"),
                            }
                            match &stmts[1] {
                                AstNode::Return(Some(expr)) => match expr.as_ref() {
                                    AstNode::MemberAccess {
                                        member,
                                        through_pointer,
                                        ..
                                    } => {
                                        assert_eq!(member, "x");
                                        assert!(!through_pointer);
                                    }
                                    _ => panic!("Expected member access"),
                                },
                                _ => panic!("Expected return statement"),
                            }
                        }
                        _ => panic!("Expected block"),
                    },
                    _ => panic!("Expected function"),
                }
            }
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_sizeof_type() {
        let mut lexer = Lexer::new("int main() { return sizeof(int); }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => match &stmts[0] {
                        AstNode::Return(Some(expr)) => match expr.as_ref() {
                            AstNode::SizeOfType(ty) => assert_eq!(ty, &Type::Int),
                            _ => panic!("Expected sizeof(type)"),
                        },
                        _ => panic!("Expected return"),
                    },
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_char_variable() {
        let mut lexer = Lexer::new("int main() { char c = 1; return c; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => match &stmts[0] {
                        AstNode::VarDecl { name, var_type, .. } => {
                            assert_eq!(name, "c");
                            assert_eq!(var_type, &Type::Char);
                        }
                        _ => panic!("Expected char var decl"),
                    },
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_unsigned_int_variable() {
        let mut lexer = Lexer::new("int main() { unsigned int u = 1; return u; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => match &stmts[0] {
                        AstNode::VarDecl { name, var_type, .. } => {
                            assert_eq!(name, "u");
                            assert_eq!(var_type, &Type::UInt);
                        }
                        _ => panic!("Expected unsigned int var decl"),
                    },
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_unsigned_char_short_long() {
        let mut lexer = Lexer::new(
            "int main() { unsigned char c = 1; unsigned short s = 2; unsigned long l = 3; return 0; }",
        );
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => {
                        match &stmts[0] {
                            AstNode::VarDecl { var_type, .. } => {
                                assert_eq!(var_type, &Type::UChar);
                            }
                            _ => panic!("Expected unsigned char var decl"),
                        }
                        match &stmts[1] {
                            AstNode::VarDecl { var_type, .. } => {
                                assert_eq!(var_type, &Type::UShort);
                            }
                            _ => panic!("Expected unsigned short var decl"),
                        }
                        match &stmts[2] {
                            AstNode::VarDecl { var_type, .. } => {
                                assert_eq!(var_type, &Type::ULong);
                            }
                            _ => panic!("Expected unsigned long var decl"),
                        }
                    }
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_short_long_variables() {
        let mut lexer = Lexer::new(
            "int main() { short s = 1; long l = 2; return 0; }",
        );
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => {
                        match &stmts[0] {
                            AstNode::VarDecl { var_type, .. } => {
                                assert_eq!(var_type, &Type::Short);
                            }
                            _ => panic!("Expected short var decl"),
                        }
                        match &stmts[1] {
                            AstNode::VarDecl { var_type, .. } => {
                                assert_eq!(var_type, &Type::Long);
                            }
                            _ => panic!("Expected long var decl"),
                        }
                    }
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }

    #[test]
    fn test_parse_compound_assignment_statement() {
        let mut lexer = Lexer::new("int main() { int x = 1; x += 2; return x; }");
        let tokens = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        match ast {
            AstNode::Program(funcs) => match &funcs[0] {
                AstNode::Function { body, .. } => match body.as_ref() {
                    AstNode::Block(stmts) => match &stmts[1] {
                        AstNode::Assignment { name, value } => {
                            assert_eq!(name, "x");
                            match value.as_ref() {
                                AstNode::BinaryOp { op: BinOp::Add, .. } => {}
                                _ => panic!("Expected addition in compound assignment"),
                            }
                        }
                        _ => panic!("Expected assignment statement"),
                    },
                    _ => panic!("Expected block"),
                },
                _ => panic!("Expected function"),
            },
            _ => panic!("Expected program"),
        }
    }
}
