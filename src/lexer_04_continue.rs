#[derive(PartialEq, Debug, Clone)]
enum Tk {
    //? Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    SEMI,
    EOF,
    IDENT(String),
    INT(String),
    ILEGAL,
    //? keywords
    FUNCTION,
    LET,
    IF,
    ELSE,
    TRUE,
    FALSE,
    RETURN,
    //? TWO_CHARS
    EQ,
    NOTEQ,
}

//? @see https://doc.rust-lang.org/stable/std/convert/trait.TryFrom.html
impl TryFrom<Byte> for Tk {
    type Error = &'static str;

    fn try_from(value: Byte) -> Result<Self, Self::Error> {
        match value {
            b'=' => Ok(Tk::ASSIGN),
            b';' => Ok(Tk::SEMI),
            b'(' => Ok(Tk::LPAREN),
            b')' => Ok(Tk::RPAREN),
            b',' => Ok(Tk::COMMA),
            b'+' => Ok(Tk::PLUS),
            b'{' => Ok(Tk::LBRACE),
            b'}' => Ok(Tk::RBRACE),
            0 => Ok(Tk::EOF),
            _ => Err("only accepts values greater than zero!"),
        }
    }
}

impl From<Tk> for char {
    fn from(value: Tk) -> char {
        match value {
            Tk::ASSIGN => '=',
            Tk::SEMI => ';',
            Tk::LPAREN => '(',
            Tk::RPAREN => ')',
            Tk::COMMA => ',',
            Tk::PLUS => '+',
            Tk::LBRACE => '{',
            Tk::RBRACE => '}',
            Tk::EOF => '\0',
            Tk::IDENT(_) => todo!(),
            Tk::INT(_) => todo!(),
            Tk::ILEGAL => todo!(),
            Tk::MINUS => todo!(),
            Tk::BANG => todo!(),
            Tk::ASTERISK => todo!(),
            Tk::SLASH => todo!(),
            Tk::LT => todo!(),
            Tk::GT => todo!(),
            //? keywords
            Tk::FUNCTION => todo!(),
            Tk::LET => todo!(),
            Tk::IF => todo!(),
            Tk::ELSE => todo!(),
            Tk::TRUE => todo!(),
            Tk::FALSE => todo!(),
            Tk::RETURN => todo!(),
            Tk::EQ => todo!(),
            Tk::NOTEQ => todo!(),
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

    fn next_tok(&mut self) -> Tk {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }

        let tok = match self.ch {
            //? operators
            b'=' => {
                if self.next_byte() == b'=' {
                    self.read_char();
                    Tk::EQ
                } else {
                    Tk::ASSIGN
                }
            }
            b'!' => {
                if self.next_byte() == b'=' {
                    self.read_char();
                    Tk::NOTEQ
                } else {
                    Tk::BANG
                }
            }
            b'+' => Tk::PLUS,
            b'-' => Tk::MINUS,
            b'*' => Tk::ASTERISK,
            b'/' => Tk::SLASH,
            b'<' => Tk::LT,
            b'>' => Tk::GT,
            b';' => Tk::SEMI,
            b'(' => Tk::LPAREN,
            b')' => Tk::RPAREN,
            b',' => Tk::COMMA,
            b'{' => Tk::LBRACE,
            b'}' => Tk::RBRACE,
            b'A'..=b'Z' => {
                let name = self.read_identifier();
                return match name.as_str() {
                    //? Keywords
                    "true" => Tk::TRUE,
                    "false" => Tk::FALSE,
                    "if" => Tk::IF,
                    "else" => Tk::ELSE,
                    "return" => Tk::RETURN,
                    "let" => Tk::LET,
                    "fn" => Tk::FUNCTION,
                    _ => Tk::IDENT(name),
                };
            }
            b'a'..=b'z' => {
                let name = self.read_identifier();
                return match name.as_str() {
                    //? Keywords
                    "true" => Tk::TRUE,
                    "false" => Tk::FALSE,
                    "if" => Tk::IF,
                    "else" => Tk::ELSE,
                    "return" => Tk::RETURN,
                    "let" => Tk::LET,
                    "fn" => Tk::FUNCTION,
                    _ => Tk::IDENT(name),
                };
            }
            b'_' => {
                let name = self.read_identifier();
                return match name.as_str() {
                    //? Keywords
                    "true" => Tk::TRUE,
                    "false" => Tk::FALSE,
                    "if" => Tk::IF,
                    "else" => Tk::ELSE,
                    "return" => Tk::RETURN,
                    "let" => Tk::LET,
                    "fn" => Tk::FUNCTION,
                    _ => Tk::IDENT(name),
                };
            }
            b'0'..=b'9' => {
                let value = self.read_number();
                return Tk::INT(value);
            }
            //todo: floats
            //todo: hex
            //todo: octal
            0 => Tk::EOF,
            _ => Tk::ILEGAL,
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
        // todo: mejorar esta vaina 👀
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
        // todo: mejorar esta vaina 👀
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

    fn next_byte(&self) -> Byte {
        if self.next_position >= self.input.len() {
            0
        } else {
            self.input[self.next_position]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "=+(){},;";
        let tests = [
            (Tk::ASSIGN, '='),
            (Tk::PLUS, '+'),
            (Tk::LPAREN, '('),
            (Tk::RPAREN, ')'),
            (Tk::LBRACE, '{'),
            (Tk::RBRACE, '}'),
            (Tk::COMMA, ','),
            (Tk::SEMI, ';'),
            (Tk::EOF, '\0'),
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
            Tk::LET,
            Tk::IDENT("five".to_string()),
            Tk::ASSIGN,
            Tk::INT("5".to_string()),
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("ten".to_string()),
            Tk::ASSIGN,
            Tk::INT("10".to_string()),
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("add".to_string()),
            Tk::ASSIGN,
            Tk::FUNCTION,
            Tk::LPAREN,
            Tk::IDENT("x".to_string()),
            Tk::COMMA,
            Tk::IDENT("y".to_string()),
            Tk::RPAREN,
            Tk::LBRACE,
            Tk::IDENT("x".to_string()),
            Tk::PLUS,
            Tk::IDENT("y".to_string()),
            Tk::SEMI,
            Tk::RBRACE,
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("result".to_string()),
            Tk::ASSIGN,
            Tk::IDENT("add".to_string()),
            Tk::LPAREN,
            Tk::IDENT("five".to_string()),
            Tk::COMMA,
            Tk::IDENT("ten".to_string()),
            Tk::RPAREN,
            Tk::SEMI,
            Tk::EOF,
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_code_operators() {
        let input = "
        let five = 5;
        let ten = 10;

        !-/*5;
        5 < 10 > 5;
        ";

        let tests = [
            Tk::LET,
            Tk::IDENT("five".to_string()),
            Tk::ASSIGN,
            Tk::INT("5".to_string()),
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("ten".to_string()),
            Tk::ASSIGN,
            Tk::INT("10".to_string()),
            Tk::SEMI,
            Tk::BANG,
            Tk::MINUS,
            Tk::SLASH,
            Tk::ASTERISK,
            Tk::INT("5".to_string()),
            Tk::SEMI,
            Tk::INT("5".to_string()),
            Tk::LT,
            Tk::INT("10".to_string()),
            Tk::GT,
            Tk::INT("5".to_string()),
            Tk::SEMI,
            Tk::EOF,
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_more_keywords() {
        let input = "
        let five = 5;
        let ten = 10;

        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        ";

        let tests = [
            Tk::LET,
            Tk::IDENT("five".to_string()),
            Tk::ASSIGN,
            Tk::INT("5".to_string()),
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("ten".to_string()),
            Tk::ASSIGN,
            Tk::INT("10".to_string()),
            Tk::SEMI,
            Tk::BANG,
            Tk::MINUS,
            Tk::SLASH,
            Tk::ASTERISK,
            Tk::INT("5".to_string()),
            Tk::SEMI,
            Tk::INT("5".to_string()),
            Tk::LT,
            Tk::INT("10".to_string()),
            Tk::GT,
            Tk::INT("5".to_string()),
            Tk::SEMI,
            Tk::IF,
            Tk::LPAREN,
            Tk::INT("5".to_string()),
            Tk::LT,
            Tk::INT("10".to_string()),
            Tk::RPAREN,
            Tk::LBRACE,
            Tk::RETURN,
            Tk::TRUE,
            Tk::SEMI,
            Tk::RBRACE,
            Tk::ELSE,
            Tk::LBRACE,
            Tk::RETURN,
            Tk::FALSE,
            Tk::SEMI,
            Tk::RBRACE,
            Tk::EOF,
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_two_character_tokens() {
        let input = "
        10 == 10;
        10 != 9;
        ";

        let tests = [
            Tk::INT("10".to_string()),
            Tk::EQ,
            Tk::INT("10".to_string()),
            Tk::SEMI,
            Tk::INT("10".to_string()),
            Tk::NOTEQ,
            Tk::INT("9".to_string()),
            Tk::SEMI,
            Tk::EOF,
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }
}
