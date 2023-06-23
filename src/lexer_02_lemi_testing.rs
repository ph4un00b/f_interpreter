#[derive(PartialEq, Debug, Clone)]
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

impl TryFrom<char> for Token {
    type Error = &'static str;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '=' => Ok(Token::ASSIGN),
            ';' => Ok(Token::SEMICOLON),
            '(' => Ok(Token::LPAREN),
            ')' => Ok(Token::RPAREN),
            ',' => Ok(Token::COMMA),
            '+' => Ok(Token::PLUS),
            '{' => Ok(Token::LBRACE),
            '}' => Ok(Token::RBRACE),
            '\0' => Ok(Token::EOF),
            _ => Err("GreaterThanZero only accepts values greater than zero!"),
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

#[derive(Debug)]
pub struct Lexer {
    pub input: String,
    current_position: u32,
    next_position: u32,
    ch: Option<char>,
}
impl Lexer {
    fn next_tok(&mut self) -> Token {
        let tok = Token::try_from(self.ch.unwrap()).unwrap();
        self.read_char();

        tok
    }

    fn read_char(&mut self) {
        if self.next_position >= self.input.len() as u32 {
            self.ch = Some('\0');
        } else {
            let pos: usize = self.next_position.try_into().unwrap();
            let Some(char)= self.input.bytes().nth(pos) else {return;};
            self.ch = Some(char as char);
        };
        self.current_position = self.next_position;
        self.next_position += 1;
    }
}

pub fn new(input: impl Into<String>) -> Lexer {
    let mut l = Lexer {
        input: input.into(),
        current_position: 0,
        next_position: 0,
        ch: None,
    };
    l.read_char();
    l
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

        let mut lex = new(input);

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok.clone(), expected_token);
            assert_eq!(char::from(tok), expected_literal);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }
}
