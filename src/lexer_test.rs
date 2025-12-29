#[cfg(test)]
mod lexer_tests {
    use crate::lexer::*;

    #[test]
    fn test_simple_tokens() {
        let mut lexer = Lexer::new("int main ( ) { return 42 ; }");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::Int);
        assert_eq!(tokens[1].token, Token::Identifier("main".to_string()));
        assert_eq!(tokens[2].token, Token::OpenParen);
        assert_eq!(tokens[3].token, Token::CloseParen);
        assert_eq!(tokens[4].token, Token::OpenBrace);
        assert_eq!(tokens[5].token, Token::Return);
        assert_eq!(tokens[6].token, Token::IntLiteral(42, IntSuffix::None));
        assert_eq!(tokens[7].token, Token::Semicolon);
        assert_eq!(tokens[8].token, Token::CloseBrace);
        assert_eq!(tokens[9].token, Token::Eof);
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ - * /");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::Plus);
        assert_eq!(tokens[1].token, Token::Minus);
        assert_eq!(tokens[2].token, Token::Star);
        assert_eq!(tokens[3].token, Token::Slash);
    }

    #[test]
    fn test_pointer_and_array_tokens() {
        let mut lexer = Lexer::new("& * [ ]");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::Ampersand);
        assert_eq!(tokens[1].token, Token::Star);
        assert_eq!(tokens[2].token, Token::OpenBracket);
        assert_eq!(tokens[3].token, Token::CloseBracket);
    }

    #[test]
    fn test_struct_tokens() {
        let mut lexer = Lexer::new("struct enum . ->");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::Struct);
        assert_eq!(tokens[1].token, Token::Enum);
        assert_eq!(tokens[2].token, Token::Dot);
        assert_eq!(tokens[3].token, Token::Arrow);
    }

    #[test]
    fn test_sizeof_token() {
        let mut lexer = Lexer::new("sizeof");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::Sizeof);
    }

    #[test]
    fn test_char_token() {
        let mut lexer = Lexer::new("char");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::Char);
    }

    #[test]
    fn test_unsigned_token() {
        let mut lexer = Lexer::new("unsigned");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::Unsigned);
    }

    #[test]
    fn test_short_long_tokens() {
        let mut lexer = Lexer::new("short long");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::Short);
        assert_eq!(tokens[1].token, Token::Long);
    }

    #[test]
    fn test_compound_assignment_tokens() {
        let mut lexer = Lexer::new("+= -= *= /= %= <<= >>= &= |= ^=");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::PlusEquals);
        assert_eq!(tokens[1].token, Token::MinusEquals);
        assert_eq!(tokens[2].token, Token::StarEquals);
        assert_eq!(tokens[3].token, Token::SlashEquals);
        assert_eq!(tokens[4].token, Token::PercentEquals);
        assert_eq!(tokens[5].token, Token::LessLessEquals);
        assert_eq!(tokens[6].token, Token::GreaterGreaterEquals);
        assert_eq!(tokens[7].token, Token::AmpersandEquals);
        assert_eq!(tokens[8].token, Token::PipeEquals);
        assert_eq!(tokens[9].token, Token::CaretEquals);
    }

    #[test]
    fn test_bitwise_tokens() {
        let mut lexer = Lexer::new("& | ^ ~ << >> %");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token, Token::Ampersand);
        assert_eq!(tokens[1].token, Token::Pipe);
        assert_eq!(tokens[2].token, Token::Caret);
        assert_eq!(tokens[3].token, Token::Tilde);
        assert_eq!(tokens[4].token, Token::LessLess);
        assert_eq!(tokens[5].token, Token::GreaterGreater);
        assert_eq!(tokens[6].token, Token::Percent);
    }
}
