//? agregar los sources activan las ayudas del vscode 💃
mod cli;
mod lexer_01;
mod lexer_02_lemi_testing;
mod lexer_03_refactored;
mod lexer_04_continue;
mod lexer_05_refactor;
mod lexer_06_scanner;
mod scanner_01;

#[cfg(test)]
mod tests {
    use crate::lexer_01::Lexer;
    use crate::lexer_01::TokenType;

    #[test]
    fn it_works() {
        let input = "=+(),{};";

        let tests = [
            (TokenType::ASSIGN, b'='),
            (TokenType::PLUS, b'+'),
            (TokenType::LPAREN, b'('),
            (TokenType::RPAREN, b')'),
            (TokenType::COMMA, b','),
            (TokenType::LBRACE, b'{'),
            (TokenType::RBRACE, b'}'),
            (TokenType::SEMICOLON, b';'),
            (TokenType::EOF, b'\0'),
        ];

        let mut lex = Lexer::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok.kind, expected_token);
            println!("💃 {:?}, expected {:?}", tok.kind, expected_token);
            println!("🕺 {:?}, expected {:?}", tok.literal, expected_literal);
            assert_eq!(tok.literal, expected_literal);
        }
    }
}
