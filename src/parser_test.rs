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
}
