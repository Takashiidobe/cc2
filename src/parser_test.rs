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
            AstNode::Program(funcs) => {
                match &funcs[0] {
                    AstNode::Function { body, .. } => {
                        match body.as_ref() {
                            AstNode::Block(stmts) => {
                                match &stmts[0] {
                                    AstNode::Return(Some(expr)) => {
                                        match expr.as_ref() {
                                            AstNode::BinaryOp { op, .. } => {
                                                assert_eq!(*op, BinOp::Add);
                                            }
                                            _ => panic!("Expected binary op"),
                                        }
                                    }
                                    _ => panic!("Expected return statement"),
                                }
                            }
                            _ => panic!("Expected block"),
                        }
                    }
                    _ => panic!("Expected function"),
                }
            }
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
            AstNode::Program(funcs) => {
                match &funcs[0] {
                    AstNode::Function { body, .. } => {
                        match body.as_ref() {
                            AstNode::Block(stmts) => {
                                match &stmts[0] {
                                    AstNode::Return(Some(expr)) => {
                                        match expr.as_ref() {
                                            AstNode::BinaryOp { op: BinOp::Add, left, right } => {
                                                match left.as_ref() {
                                                    AstNode::IntLiteral(2) => {},
                                                    _ => panic!("Expected left to be 2"),
                                                }
                                                match right.as_ref() {
                                                    AstNode::BinaryOp { op: BinOp::Multiply, .. } => {},
                                                    _ => panic!("Expected right to be multiplication"),
                                                }
                                            }
                                            _ => panic!("Expected addition at top level"),
                                        }
                                    }
                                    _ => panic!("Expected return statement"),
                                }
                            }
                            _ => panic!("Expected block"),
                        }
                    }
                    _ => panic!("Expected function"),
                }
            }
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
            AstNode::Program(funcs) => {
                match &funcs[0] {
                    AstNode::Function { body, .. } => {
                        match body.as_ref() {
                            AstNode::Block(stmts) => {
                                match &stmts[0] {
                                    AstNode::Return(Some(expr)) => {
                                        match expr.as_ref() {
                                            AstNode::BinaryOp { op: BinOp::Multiply, left, right } => {
                                                match left.as_ref() {
                                                    AstNode::BinaryOp { op: BinOp::Add, .. } => {},
                                                    _ => panic!("Expected left to be addition"),
                                                }
                                                match right.as_ref() {
                                                    AstNode::IntLiteral(4) => {},
                                                    _ => panic!("Expected right to be 4"),
                                                }
                                            }
                                            _ => panic!("Expected multiplication at top level"),
                                        }
                                    }
                                    _ => panic!("Expected return statement"),
                                }
                            }
                            _ => panic!("Expected block"),
                        }
                    }
                    _ => panic!("Expected function"),
                }
            }
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
            AstNode::Program(funcs) => {
                match &funcs[0] {
                    AstNode::Function { body, .. } => {
                        match body.as_ref() {
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
                        }
                    }
                    _ => panic!("Expected function"),
                }
            }
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
            AstNode::Program(funcs) => {
                match &funcs[0] {
                    AstNode::Function { body, .. } => {
                        match body.as_ref() {
                            AstNode::Block(stmts) => {
                                match &stmts[0] {
                                    AstNode::Return(Some(expr)) => {
                                        match expr.as_ref() {
                                            AstNode::FunctionCall { name, args } => {
                                                assert_eq!(name, "add");
                                                assert_eq!(args.len(), 2);
                                            }
                                            _ => panic!("Expected function call"),
                                        }
                                    }
                                    _ => panic!("Expected return"),
                                }
                            }
                            _ => panic!("Expected block"),
                        }
                    }
                    _ => panic!("Expected function"),
                }
            }
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
            AstNode::Program(funcs) => {
                match &funcs[0] {
                    AstNode::Function { name, params, .. } => {
                        assert_eq!(name, "add");
                        assert_eq!(params.len(), 2);
                        assert_eq!(params[0].name, "a");
                        assert_eq!(params[1].name, "b");
                    }
                    _ => panic!("Expected function"),
                }
            }
            _ => panic!("Expected program"),
        }
    }
}
