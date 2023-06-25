use std::collections::HashMap;

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
/*
 * este trait ya no lo usamos, mucha lógica
 * intermedia quedó en el lexer
 * pero dejo este code por referencia.
 * usage: let tok = Tk::try_from(b'=').unwrap();
 */
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

/*
 * este trait sirve para el testing muy básico
 * pero la lógica aquí
 * es la lógica repetida superficialmente en Lex#next_tok
 * investigar si hay algún helper
 * que voltee los datos
 *
 * usage: String::from(Tk::SEMI) -> ";"
 */
impl From<Tk> for String {
    fn from(value: Tk) -> String {
        match value {
            Tk::ASSIGN => "=".to_string(),
            Tk::SEMI => ";".to_string(),
            Tk::LPAREN => "(".to_string(),
            Tk::RPAREN => ")".to_string(),
            Tk::COMMA => ",".to_string(),
            Tk::PLUS => "+".to_string(),
            Tk::LBRACE => "{".to_string(),
            Tk::RBRACE => "}".to_string(),
            Tk::EOF => "\0".to_string(),
            //? operators
            Tk::MINUS => "-".to_string(),
            Tk::BANG => "!".to_string(),
            Tk::ASTERISK => "*".to_string(),
            Tk::SLASH => "/".to_string(),
            Tk::LT => "<".to_string(),
            Tk::GT => ">".to_string(),
            //? keywords
            Tk::FUNCTION => "fn".to_string(),
            Tk::LET => "let".to_string(),
            Tk::IF => "if".to_string(),
            Tk::ELSE => "else".to_string(),
            Tk::TRUE => "true".to_string(),
            Tk::FALSE => "false".to_string(),
            Tk::RETURN => "return".to_string(),
            //? two-character
            Tk::EQ => "==".to_string(),
            Tk::NOTEQ => "!=".to_string(),
            //? special
            Tk::IDENT(_) => todo!(),
            Tk::INT(_) => todo!(),
            Tk::ILEGAL => todo!(),
        }
    }
}

type Byte = u8;
#[derive(Debug)]
pub struct Lexer {
    pub had_error: bool,
    pub had_runtime_error: bool,
    //? esta propiedad deberia borrarse, para no repetir en memoria
    tokens: Vec<Tk>,
}

struct Scanner {
    source_code: Vec<Byte>,
    tokens: Vec<Tk>,
    current_position: usize,
    next_position: usize,
    current_character: Byte,
    line: i32,
    // keywords: HashMap<String, Tk>,
}

impl Scanner {
    pub fn new(source_code: String) -> Self {
        let mut l = Self {
            source_code: source_code.into_bytes(),
            current_position: 0,
            next_position: 0,
            current_character: 0,
            line: 1,
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

    fn scan_tokens(&mut self) -> &Vec<Tk> {
        while self.current_position <= self.source_code.len() {
            let token = self.next_tok();
            self.tokens.push(token);
        }

        &self.tokens
    }

    fn next_tok(&mut self) -> Tk {
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
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
                let name = self.read_identifier();
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

        self.read_character();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let position = self.current_position;
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
        let vec = self.source_code[position..self.current_position].to_vec();
        // todo: mejorar esta vaina 👀
        let x = String::from_utf8(vec).unwrap();

        x
    }

    fn next_byte(&self) -> Byte {
        if self.next_position >= self.source_code.len() {
            0
        } else {
            self.source_code[self.next_position]
        }
    }

    fn read_number(&mut self) -> String {
        let position = self.current_position;
        while self.current_character.is_ascii_digit() {
            self.read_character();
        }

        let vec = self.source_code[position..self.current_position].to_vec();
        // todo: mejorar esta vaina 👀
        let x = String::from_utf8(vec).unwrap();

        x
    }
}

impl Lexer {
    pub fn new(source_code: String) -> Self {
        let mut scanner = Scanner::new(source_code);
        Self {
            had_error: false,
            had_runtime_error: false,
            tokens: scanner.scan_tokens().clone(),
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
    fn it_can_scan() {
        let input = "=+(){},;";
        let expected_tokens = vec![
            Tk::ASSIGN,
            Tk::PLUS,
            Tk::LPAREN,
            Tk::RPAREN,
            Tk::LBRACE,
            Tk::RBRACE,
            Tk::COMMA,
            Tk::SEMI,
            Tk::EOF,
        ];

        let mut lex = Lexer::new(input.into());
        assert_eq!(lex.tokens, expected_tokens);
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
}
