use std::collections::HashMap;

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
    LET,
    IDENT(String),
    INT(String),
    FUNCTION,
    ILEGAL,
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
            Token::LET => todo!(),
            Token::FUNCTION => todo!(),
            Token::IDENT(_) => todo!(),
            Token::INT(_) => todo!(),
            Token::ILEGAL => todo!(),
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
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }

        let tok = match self.ch {
            b'=' => Token::ASSIGN,
            b';' => Token::SEMICOLON,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b',' => Token::COMMA,
            b'+' => Token::PLUS,
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            b'A'..=b'Z' => {
                let name = self.read_identifier();
                return match name.as_str() {
                    "let" => Token::LET,
                    "fn" => Token::FUNCTION,
                    _ => Token::IDENT(name),
                };
            }
            b'a'..=b'z' => {
                let name = self.read_identifier();
                return match name.as_str() {
                    "let" => Token::LET,
                    "fn" => Token::FUNCTION,
                    _ => Token::IDENT(name),
                };
            }
            b'_' => {
                let name = self.read_identifier();
                return match name.as_str() {
                    "let" => Token::LET,
                    "fn" => Token::FUNCTION,
                    _ => Token::IDENT(name),
                };
            }
            b'0'..=b'9' => {
                let digit = self.read_number();
                return Token::INT(digit);
            }
            //todo: floats
            //todo: hex
            //todo: octal
            0 => Token::EOF,
            _ => Token::ILEGAL,
        };

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

    fn read_identifier(&mut self) -> String {
        let position = self.current_position;
        /*
         * usage:
         * allow snake_case
         * allow ? for query methods f.i.: jamon?
         * allow ! for throwing methods f.i.: jamon!
         */
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' || self.ch == b'?' || self.ch == b'!'
        {
            self.read_char();
        }
        let vec = self.input[position..self.current_position].to_vec();
        let x = String::from_utf8(vec).unwrap();
        println!(
            "🎈 {} - {}, {}, 👀 next {}",
            position,
            self.current_position,
            x,
            self.input[self.current_position + 1]
        );
        x
    }

    fn read_number(&mut self) -> String {
        let position = self.current_position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        let vec = self.input[position..self.current_position].to_vec();
        let x = String::from_utf8(vec).unwrap();
        println!(
            "🎈 {} - {}, {}, 👀 next {}",
            position,
            self.current_position,
            x,
            self.input[self.current_position + 1]
        );
        x
    }
}

fn is_letter(ch: u8) -> bool {
    ch.is_ascii_alphabetic() || ch == b'_'
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
    #[test]
    fn it_reads_source_code() {
        let input = "
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        ";

        let tests = [
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT("10".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }
}
