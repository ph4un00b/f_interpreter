#[derive(PartialEq, Debug)]
enum Token {
    ASSIGN,
    PLUS,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    SEMICOLON,
    EOF,
}

//? @see https://doc.rust-lang.org/stable/std/convert/trait.TryFrom.html
impl TryFrom<Byte> for Token {
    type Error = &'static str;

    fn try_from(value: Byte) -> Result<Self, Self::Error> {
        match value {
            b'=' => Ok(Token::ASSIGN),
            b';' => Ok(Token::SEMICOLON),
            b'(' => Ok(Token::LPAREN),
            b')' => Ok(Token::RPAREN),
            b',' => Ok(Token::COMMA),
            b'+' => Ok(Token::PLUS),
            b'{' => Ok(Token::LBRACE),
            b'}' => Ok(Token::RBRACE),
            0 => Ok(Token::EOF),
            _ => Err("only accepts values greater than zero!"),
        }
    }
}

impl From<Token> for char {
    fn from(value: Token) -> char {
        match value {
            Token::ASSIGN => '=',
            Token::SEMICOLON => ';',
            Token::LPAREN => '(',
            Token::RPAREN => ')',
            Token::COMMA => ',',
            Token::PLUS => '+',
            Token::LBRACE => '{',
            Token::RBRACE => '}',
            Token::EOF => '\0',
        }
    }
}

type Byte = u8;
#[derive(Debug)]
pub struct Lexer {
    pub input: Vec<Byte>,
    current_position: usize,
    next_position: usize,
    ch: Byte,
}
impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Self {
            input: input.into_bytes(),
            current_position: 0,
            next_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    fn next_tok(&mut self) -> Token {
        let tok = Token::try_from(self.ch).unwrap();
        self.read_char();

        tok
    }

    fn read_char(&mut self) {
        if self.next_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.next_position];
        };
        self.current_position = self.next_position;
        self.next_position += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "=+(){},;";
        let tests = [
            (Token::ASSIGN, '='),
            (Token::PLUS, '+'),
            (Token::LPAREN, '('),
            (Token::RPAREN, ')'),
            (Token::LBRACE, '{'),
            (Token::RBRACE, '}'),
            (Token::COMMA, ','),
            (Token::SEMICOLON, ';'),
            (Token::EOF, '\0'),
        ];

        let mut lex = Lexer::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
            assert_eq!(char::from(tok), expected_literal);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }
}
