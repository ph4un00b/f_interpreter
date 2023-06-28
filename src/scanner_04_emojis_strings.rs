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
struct TkMeta {
    value: String,
    line: i32,
}

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
    STR(String, i32),
    IDENT(String, i32),
    INT(String, i32),
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
    EMOJI4(String, i32),
    EMOJI3(String, i32),
    EMOJI2(String, i32),
    EMOJI1(String, i32),
}

pub type Byte = u8;

pub struct Scanner {
    source_code: Vec<Byte>,
    tokens: Vec<Tk>,
    current_position: usize,
    next_position: usize,
    current_byte: Byte,
    current_line: i32,
    // keywords: HashMap<String, Tk>,
}

impl Scanner {
    pub fn new(source_code: String) -> Self {
        let code = source_code.into_bytes();
        println!("🎀{:?}", code);
        let mut l = Self {
            source_code: code,
            current_position: 0,
            next_position: 0,
            current_byte: 0,
            /*
             * Tracking source locations
             * @see https://www3.nd.edu/~dthain/compilerbook/compilerbook.pdf
             *
             * In later stages of the compiler,
             * it is useful for the parser or typechecker to know exactly
             * what line and column number a token was located at,
             * usually to print out a helpful error message.
             * (“Undefined symbol spider at line 153.”)
             * This is easily done by having the scanner match newline
             * characters, and increase the line count
             * (but not return a token) each time one is found.
             */
            current_line: 1,
            tokens: Vec::new(),
        };
        l.read_byte();
        l
    }

    fn read_byte(&mut self) {
        self.current_byte = if self.next_position >= self.source_code.len() {
            0
        } else {
            self.source_code[self.next_position]
        };

        // println!(
        //     "🎄 {}::{}",
        //     self.current_byte,
        //     char::from(self.current_byte)
        // );
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
        // todo: crear un next function que englobe el salto de linea ❓👀
        /*
         * habría beneficio? o más desventaja 🤔
         * esta es la lógica que tenemos en self#read_byte
         *
         * valdría la pena agregar el
         *  self.current_line += 1;❓
         */

        //? @see https://github.com/DoctorWkt/acwj/blob/master/01_Scanner/Readme.md#functions-in-scanc

        //? Get the next character from the input file.
        //? static int next(void) {
        //?     int c;
        //?     if (Putback) {                //? Use the character put
        //?       c = Putback;                //? back if there is one
        //?       Putback = 0;
        //?       return c;
        //?     }
        //?     c = fgetc(Infile);            //? Read from input file
        //?     if ('\n' == c)
        //?       Line++;                     //? Increment line count
        //?     return c;
        //?   }

        while self.current_byte.is_ascii_whitespace() {
            if let b'\n' = self.current_byte {
                self.current_line += 1;
            }
            self.read_byte();
        }

        let tok = match self.current_byte {
            //? operators
            b'=' => {
                if self.next_byte() == b'=' {
                    self.read_byte();
                    Tk::EQ
                } else {
                    Tk::ASSIGN
                }
            }
            b'!' => {
                if self.next_byte() == b'=' {
                    self.read_byte();
                    Tk::NOTEQ
                } else {
                    Tk::BANG
                }
            }
            b'/' => {
                if self.next_byte() == b'/' {
                    while self.next_byte() != b'\n' && self.next_byte() != 0 {
                        self.read_byte();
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
            b'"' => {
                //todo: buscar una librería para los emojis
                //? @see https://unicode.org/emoji/techindex.html
                //? @see https://unicode.org/Public/emoji/15.0/
                let start = self.current_position;

                while (self.current_byte.is_ascii()
                    || self.next_byte().is_ascii_whitespace()
                    // || matches!(self.current_byte, 240..=244 | 224..=239))
                    || matches!(self.current_byte, 224..=244))
                    && self.next_byte() != b'"'
                {
                    match self.current_byte {
                        // todo: estoy asumiendo los emojis de 3 al menos
                        // 240..=244 | 224..=239 => {
                        224..=244 => {
                            /*
                             * for strings  => "hola 😍🌎 mundo"
                             */
                            // todo: estos rangos asumen los bytes que contienes los emojis
                            // todo: seguramente hay emojis fuera ❌
                            while (self.next_byte() >= 128 && self.next_byte() <= 191/* bytes que componen el emoji */)
                                || (self.next_byte() >= 224 && self.next_byte() <= 244/* o vuelve a encontrarse con nuevo emoji */)
                            {
                                self.read_byte();
                            }
                        }
                        b'\n' => {
                            //? multiline strings supported!
                            /*
                             * for strings
                             * => "hola
                             * 😍🌎
                             * mundo"
                             */
                            self.current_line += 1;
                            self.read_byte();
                        }
                        _ => self.read_byte(),
                    }
                }

                // todo
                // if self.current_position >= self.source_code.len() {
                //     println!("❌ Unterminated string.");
                //     return Tk::ILEGAL;
                // }

                self.read_byte();

                /*
                 * Cleaning tokens:
                 * @see https://www3.nd.edu/~dthain/compilerbook/compilerbook.pdf
                 *
                 * Internally, the compiler only cares about the actual
                 * contents of the string. Typically, this is accomplished
                 *  by writing a function string clean in the "postamble" of
                 *  the Flex specification. The function is invoked by the
                 *  matching rule before returning the desired token type.
                 */
                let vec = self.source_code[(start + 1)..(self.current_position)].to_vec();
                // todo: mejorar esta vaina 👀
                let x = String::from_utf8(vec).unwrap();

                /*
                 * Constraining tokens.
                 *
                 * Although regular expressions can match
                 * tokens of arbitrary length, it does not follow that
                 * a compiler must be prepared to accept them.
                 * There would be little point to accepting a 1000-letter
                 * identifier, or an integer larger than the machine’s word size.
                 * The typical approach is to set the maximum token length
                 * (YYLMAX in flex) to a very large
                 * value, then examine the token to see if it exceeds a
                 * logical limit in the action that matches the token.
                 * This allows you to emit an error message that
                 *  describes the offending token as needed.
                 */
                //todo: limitaremos tokens?
                Tk::STR(x, self.current_line)
            }
            b'0'..=b'9' => {
                let start = self.current_position;
                while self.current_byte.is_ascii_digit() {
                    self.read_byte();
                }

                let vec = self.source_code[start..self.current_position].to_vec();
                // todo: mejorar esta vaina 👀
                let value = String::from_utf8(vec).unwrap();
                return Tk::INT(value, self.current_line);
            }
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
                let start = self.current_position;
                /*
                 * usage:
                 * allow snake_case
                 * allow ? for query methods f.i.: jamon?
                 * allow ! for throwing methods f.i.: jamon!
                 */
                while self.current_byte.is_ascii_alphabetic()
                    || self.current_byte == b'_'
                    || self.current_byte == b'?'
                    || self.current_byte == b'!'
                {
                    self.read_byte();
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
                    _ => Tk::IDENT(name, self.current_line),
                };
            }
            //todo: floats
            //todo: hex
            //todo: octal
            0 => Tk::EOF,
            // 240..=244 | 224..=239 => {
            224..=244 => {
                let start = self.current_position;
                let mut num_bytes = 0;
                while num_bytes <= 4 && self.next_byte() >= 128 && self.next_byte() <= 191 {
                    self.read_byte();
                    num_bytes += 1;
                }
                let vec = self.source_code[start..(self.current_position + 1)].to_vec();
                let emoji = String::from_utf8(vec).unwrap();

                match num_bytes {
                    4 => Tk::EMOJI4(emoji, self.current_line),
                    3 => Tk::EMOJI3(emoji, self.current_line),
                    2 => Tk::EMOJI2(emoji, self.current_line),
                    1 => Tk::EMOJI1(emoji, self.current_line),
                    _ => Tk::ILEGAL,
                }
            }
            //todo: handling errors: printf("Unrecognized character %c on line %d\n", c, Line);
            _ => Tk::ILEGAL,
        };

        self.read_byte();
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
    fn it_reads_comments_on_no_newline() {
        let input = "// fn";

        let tests = [
            //?
            Tk::COMMENT,
        ];

        let mut lex = Scanner::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_emojis_as_token() {
        //todo: https://thekevinscott.com/emojis-in-javascript/#writing-a-regular-expression
        let input = "\"Hello World!\"👋🏾😍🌍🔥✅❌;";

        let tests = [
            Tk::STR("Hello World!".to_string(), 1),
            // todo: deberíamos soportar 4-bytes emojis 👋🏾?
            Tk::EMOJI3("👋".to_owned(), 1),
            Tk::EMOJI3("🏾".to_owned(), 1),
            // todo: creo estamos dejando afuera el byte de variación
            Tk::EMOJI3("😍".to_owned(), 1),
            Tk::EMOJI3("🌍".to_owned(), 1),
            Tk::EMOJI3("🔥".to_owned(), 1),
            Tk::EMOJI2("✅".to_owned(), 1),
            Tk::EMOJI2("❌".to_owned(), 1),
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
    fn it_reads_emojis_in_strings() {
        let input = "\"hola 😍🌍 mundo\";";

        let tests = [
            //?
            Tk::STR("hola 😍🌍 mundo".to_string(), 1),
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
            Tk::IDENT("five".to_string(), 2),
            Tk::ASSIGN,
            Tk::INT("5".to_string(), 2),
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("ten".to_string(), 3),
            Tk::ASSIGN,
            Tk::INT("10".to_string(), 3),
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("add".to_string(), 5),
            Tk::ASSIGN,
            Tk::FUNCTION,
            Tk::LPAREN,
            Tk::IDENT("x".to_string(), 5),
            Tk::COMMA,
            Tk::IDENT("y".to_string(), 5),
            Tk::RPAREN,
            Tk::LBRACE,
            Tk::IDENT("x".to_string(), 6),
            Tk::PLUS,
            Tk::IDENT("y".to_string(), 6),
            Tk::SEMI,
            Tk::RBRACE,
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("result".to_string(), 9),
            Tk::ASSIGN,
            Tk::IDENT("add".to_string(), 9),
            Tk::LPAREN,
            Tk::IDENT("five".to_string(), 9),
            Tk::COMMA,
            Tk::IDENT("ten".to_string(), 9),
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
            Tk::IDENT("five".to_string(), 2),
            Tk::ASSIGN,
            Tk::INT("5".to_string(), 2),
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("ten".to_string(), 3),
            Tk::ASSIGN,
            Tk::INT("10".to_string(), 3),
            Tk::SEMI,
            Tk::BANG,
            Tk::MINUS,
            Tk::SLASH,
            Tk::ASTERISK,
            Tk::INT("5".to_string(), 5),
            Tk::SEMI,
            Tk::INT("5".to_string(), 6),
            Tk::LT,
            Tk::INT("10".to_string(), 6),
            Tk::GT,
            Tk::INT("5".to_string(), 6),
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
            Tk::INT("10".to_string(), 2),
            Tk::EQ,
            Tk::INT("10".to_string(), 2),
            Tk::SEMI,
            Tk::INT("10".to_string(), 3),
            Tk::NOTEQ,
            Tk::INT("9".to_string(), 3),
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
            Tk::INT("10".to_string(), 2),
            Tk::EQ,
            Tk::INT("10".to_string(), 2),
            Tk::SEMI,
            Tk::STR("hola".to_string(), 3),
            Tk::INT("10".to_string(), 4),
            Tk::NOTEQ,
            Tk::INT("9".to_string(), 4),
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
    fn it_reads_multiline_strings() {
        //todo: check other resources for new ways to handle them
        let input = "
         10 == 10;
         \"hola
 jamon\"
         10 != 9;
         ";

        let tests = [
            Tk::INT("10".to_string(), 2),
            Tk::EQ,
            Tk::INT("10".to_string(), 2),
            Tk::SEMI,
            Tk::STR("hola\n jamon".to_string(), 4),
            Tk::INT("10".to_string(), 5),
            Tk::NOTEQ,
            Tk::INT("9".to_string(), 5),
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
            Tk::INT("10".to_string(), 2),
            Tk::EQ,
            Tk::INT("10".to_string(), 2),
            Tk::SEMI,
            Tk::STR("hola".to_string(), 3),
            Tk::COMMENT,
            Tk::INT("10".to_string(), 5),
            Tk::NOTEQ,
            Tk::INT("9".to_string(), 5),
            Tk::SEMI,
            Tk::EOF,
        ];

        let mut lex = Scanner::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            println!("{:?}", tok);
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
            Tk::IDENT("five".to_string(), 3),
            Tk::ASSIGN,
            Tk::INT("5".to_string(), 3),
            Tk::SEMI,
            Tk::LET,
            Tk::IDENT("ten".to_string(), 4),
            Tk::ASSIGN,
            Tk::INT("10".to_string(), 4),
            Tk::SEMI,
            Tk::BANG,
            Tk::MINUS,
            Tk::SLASH,
            Tk::ASTERISK,
            Tk::INT("5".to_string(), 6),
            Tk::SEMI,
            Tk::INT("5".to_string(), 7),
            Tk::LT,
            Tk::INT("10".to_string(), 7),
            Tk::GT,
            Tk::INT("5".to_string(), 7),
            Tk::SEMI,
            Tk::IF,
            Tk::LPAREN,
            Tk::INT("5".to_string(), 9),
            Tk::LT,
            Tk::INT("10".to_string(), 9),
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
