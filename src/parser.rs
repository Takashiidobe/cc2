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
            functions.push(self.parse_function()?);
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
        match self.current() {
            Token::Int => {
                self.advance();
                Ok(Type::Int)
            }
            _ => Err(format!("Expected type, got {:?}", self.current())),
        }
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        Ok(Vec::new())
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
            _ => Err(format!("Unexpected token in statement: {:?}", self.current())),
        }
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
        match self.current() {
            Token::IntLiteral(n) => {
                let val = *n;
                self.advance();
                Ok(AstNode::IntLiteral(val))
            }
            _ => Err(format!("Expected expression, got {:?}", self.current())),
        }
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

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len() || self.current() == &Token::Eof
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

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
}
