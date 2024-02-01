/*
 * This task has been variously called “scanning” and “lexing”
 * (short for “lexical analysis”) over the years.
 * Way back when computers were as big as Winnebagos but had less
 * memory than your watch, some people used “scanner” only to refer
 * to the piece of code that dealt with reading raw source code
 * characters from disk and buffering them in memory.
 * Then “lexing” was the subsequent phase that did useful stuff
 * with the characters.
 *
 * These days, reading a source file into memory is trivial,
 * so it’s rarely a distinct phase in the compiler.
 * Because of that, the two terms are basically interchangeable.
 * @see https://craftinginterpreters.com/scanning.html
 */

#[derive(PartialEq, Debug, Clone)]
pub enum Tk {
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
    STR(String),
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
    COMMENT,
}

pub type Byte = u8;

pub struct Scanner {
    source_code: Vec<Byte>,
    tokens: Vec<Tk>,
    current_position: usize,
    next_position: usize,
    current_character: Byte,
    current_line: i32,
    // keywords: HashMap<String, Tk>,
}

impl Scanner {
    pub fn new(source_code: String) -> Self {
        let mut l = Self {
            source_code: source_code.into_bytes(),
            current_position: 0,
            next_position: 0,
            current_character: 0,
            current_line: 1,
            tokens: Vec::new(),
        };
        l.read_character();
        l
    }

    fn read_character(&mut self) {
        self.current_character = if self.next_position >= self.source_code.len() {
            0
        } else {
            self.source_code[self.next_position]
        };

        self.current_position = self.next_position;
        self.next_position += 1;
    }

    pub fn scan_tokens(&mut self) -> &Vec<Tk> {
        while self.current_position <= self.source_code.len() {
            let token = self.next_tok();
            self.tokens.push(token);
        }

        &self.tokens
    }

    pub fn next_tok(&mut self) -> Tk {
        while self.current_character.is_ascii_whitespace() {
            self.read_character();
        }

        let tok = match self.current_character {
            //? operators
            b'=' => {
                if self.next_byte() == b'=' {
                    self.read_character();
                    Tk::EQ
                } else {
                    Tk::ASSIGN
                }
            }
            b'!' => {
                if self.next_byte() == b'=' {
                    self.read_character();
                    Tk::NOTEQ
                } else {
                    Tk::BANG
                }
            }
            b'/' => {
                if self.next_byte() == b'/' {
                    // todo: while self.next_byte() != b'\n' && !self.is_at_end()
                    while self.next_byte() != b'\n' {
                        self.read_character();
                    }
                    Tk::COMMENT
                } else {
                    Tk::SLASH
                }
            }
            b'+' => Tk::PLUS,
            b'-' => Tk::MINUS,
            b'*' => Tk::ASTERISK,
            b'<' => Tk::LT,
            b'>' => Tk::GT,
            b';' => Tk::SEMI,
            b'(' => Tk::LPAREN,
            b')' => Tk::RPAREN,
            b',' => Tk::COMMA,
            b'{' => Tk::LBRACE,
            b'}' => Tk::RBRACE,
            // todo: update lines
            // b'\n' =>{
            //     self.current_line += 1
            //     ()
            // },
            b'"' => {
                let start = self.current_position;
                while self.current_character.is_ascii() {
                    //todo: lines
                    // if self.next_byte() == b'\n' {
                    //     self.current_line += 1;
                    // }
                    self.read_character();
                }

                // todo
                // if self.current_position >= self.source_code.len() {
                //     println!("❌ Unterminated string.");
                //     return Tk::ILEGAL;
                // }

                self.read_character();

                let vec = self.source_code[(start + 1)..(self.current_position)].to_vec();
                // todo: mejorar esta vaina 👀
                let x = String::from_utf8(vec).unwrap();

                Tk::STR(x)
            }
            b'0'..=b'9' => {
                let start = self.current_position;
                while self.current_character.is_ascii_digit() {
                    self.read_character();
                }

                let vec = self.source_code[start..self.current_position].to_vec();
                // todo: mejorar esta vaina 👀
                let value = String::from_utf8(vec).unwrap();
                return Tk::INT(value);
            }
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
                let start = self.current_position;
                /*
                 * usage:
                 * allow snake_case
                 * allow ? for query methods f.i.: jamon?
                 * allow ! for throwing methods f.i.: jamon!
                 */
                while self.current_character.is_ascii_alphabetic()
                    || self.current_character == b'_'
                    || self.current_character == b'?'
                    || self.current_character == b'!'
                {
                    self.read_character();
                }
                let vec = self.source_code[start..self.current_position].to_vec();
                // todo: mejorar esta vaina 👀
                let name = String::from_utf8(vec).unwrap();

                return match name.as_str() {
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
            //todo: floats
            //todo: hex
            //todo: octal
            0 => Tk::EOF,
            _ => Tk::ILEGAL,
        };

        self.read_character();
        tok
    }

    fn next_byte(&self) -> Byte {
        if self.next_position >= self.source_code.len() {
            0
        } else {
            self.source_code[self.next_position]
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

        let mut lex = Scanner::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
            // println!("🎄{} - {}", String::from(tok), expected_literal.to_string());
            assert_eq!(String::from(tok), expected_literal.to_string());
        }

        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_operators() {
        let input = "=+-!*/<>";
        let tests = [
            (Tk::ASSIGN, '='),
            (Tk::PLUS, '+'),
            (Tk::MINUS, '-'),
            (Tk::BANG, '!'),
            (Tk::ASTERISK, '*'),
            (Tk::SLASH, '/'),
            (Tk::LT, '<'),
            (Tk::GT, '>'),
        ];

        let mut lex = Scanner::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
            assert_eq!(String::from(tok), expected_literal.to_string());
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_keywords() {
        let input = "
        fn
        let
        if
        else
        true
        false
        return
        ";

        let tests = [
            (Tk::FUNCTION, "fn".to_string()),
            (Tk::LET, "let".to_string()),
            (Tk::IF, "if".to_string()),
            (Tk::ELSE, "else".to_string()),
            (Tk::TRUE, "true".to_string()),
            (Tk::FALSE, "false".to_string()),
            (Tk::RETURN, "return".to_string()),
        ];

        let mut lex = Scanner::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
            assert_eq!(String::from(tok), expected_literal.to_string());
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_two_character_keywords() {
        let input = "
        ==
        !=
        ";

        let tests = [
            //?
            (Tk::EQ, "==".to_string()),
            (Tk::NOTEQ, "!=".to_string()),
        ];

        let mut lex = Scanner::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
            assert_eq!(String::from(tok), expected_literal.to_string());
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

        let mut lex = Scanner::new(input.into());

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

        let mut lex = Scanner::new(input.into());

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

        let mut lex = Scanner::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_strings() {
        let input = "
        10 == 10;
        \"hola\"
        10 != 9;
        ";

        let tests = [
            Tk::INT("10".to_string()),
            Tk::EQ,
            Tk::INT("10".to_string()),
            Tk::SEMI,
            Tk::STR("hola".to_string()),
            Tk::INT("10".to_string()),
            Tk::NOTEQ,
            Tk::INT("9".to_string()),
            Tk::SEMI,
            Tk::EOF,
        ];

        let mut lex = Scanner::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_comments() {
        let input = "
        10 == 10;
        \"hola\"
        // comentario
        10 != 9;
        ";

        let tests = [
            Tk::INT("10".to_string()),
            Tk::EQ,
            Tk::INT("10".to_string()),
            Tk::SEMI,
            Tk::STR("hola".to_string()),
            Tk::COMMENT,
            Tk::INT("10".to_string()),
            Tk::NOTEQ,
            Tk::INT("9".to_string()),
            Tk::SEMI,
            Tk::EOF,
        ];

        let mut lex = Scanner::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_more_keywords() {
        let input = "
        // inicio
        let five = 5;
        let ten = 10;

        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            // falso
            return false;
        }
        // fin
        ";

        let tests = [
            Tk::COMMENT,
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
            Tk::COMMENT,
            Tk::RETURN,
            Tk::FALSE,
            Tk::SEMI,
            Tk::RBRACE,
            Tk::COMMENT,
            Tk::EOF,
        ];

        let mut lex = Scanner::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }
}
