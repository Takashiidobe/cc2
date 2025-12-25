#[cfg(test)]
mod lexer_tests {
    use crate::lexer::*;

    #[test]
    fn test_simple_tokens() {
        let mut lexer = Lexer::new("int main ( ) { return 42 ; }");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Int);
        assert_eq!(tokens[1], Token::Identifier("main".to_string()));
        assert_eq!(tokens[2], Token::OpenParen);
        assert_eq!(tokens[3], Token::CloseParen);
        assert_eq!(tokens[4], Token::OpenBrace);
        assert_eq!(tokens[5], Token::Return);
        assert_eq!(tokens[6], Token::IntLiteral(42));
        assert_eq!(tokens[7], Token::Semicolon);
        assert_eq!(tokens[8], Token::CloseBrace);
        assert_eq!(tokens[9], Token::Eof);
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ - * /");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Plus);
        assert_eq!(tokens[1], Token::Minus);
        assert_eq!(tokens[2], Token::Star);
        assert_eq!(tokens[3], Token::Slash);
    }

    #[test]
    fn test_pointer_and_array_tokens() {
        let mut lexer = Lexer::new("& * [ ]");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Ampersand);
        assert_eq!(tokens[1], Token::Star);
        assert_eq!(tokens[2], Token::OpenBracket);
        assert_eq!(tokens[3], Token::CloseBracket);
    }

    #[test]
    fn test_struct_tokens() {
        let mut lexer = Lexer::new("struct . ->");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Struct);
        assert_eq!(tokens[1], Token::Dot);
        assert_eq!(tokens[2], Token::Arrow);
    }

    #[test]
    fn test_sizeof_token() {
        let mut lexer = Lexer::new("sizeof");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Sizeof);
    }

    #[test]
    fn test_char_token() {
        let mut lexer = Lexer::new("char");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Char);
    }

    #[test]
    fn test_unsigned_token() {
        let mut lexer = Lexer::new("unsigned");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Unsigned);
    }

    #[test]
    fn test_short_long_tokens() {
        let mut lexer = Lexer::new("short long");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0], Token::Short);
        assert_eq!(tokens[1], Token::Long);
    }
}
