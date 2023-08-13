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

#[derive(PartialEq, Debug, Clone, PartialOrd)]
pub enum Tk {
    //? Operators
    Assign,
    Plus,
    Sub,
    Bang,
    Mul,
    Div,
    LT,
    GT,

    LP,
    RP,
    LB,
    RB,
    Comma,
    Semi,
    End,
    String(String, i32),
    Ident(String, i32),
    Num(String, i32),
    None,
    //? keywords
    Func,
    Let,
    If,
    Else,
    True,
    False,
    Return,
    //? TWO_CHARS
    EQ,
    NotEq,
    Comment,
    EMOJI4(String, i32),
    EMOJI3(String, i32),
    EMOJI2(String, i32),
    EMOJI1(String, i32),
}

impl std::fmt::Display for Tk {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tk::Assign => todo!(),
            Tk::Plus => todo!(),
            Tk::Sub => todo!(),
            Tk::Bang => todo!(),
            Tk::Mul => todo!(),
            Tk::Div => todo!(),
            Tk::LT => todo!(),
            Tk::GT => todo!(),
            Tk::LP => todo!(),
            Tk::RP => todo!(),
            Tk::LB => todo!(),
            Tk::RB => todo!(),
            Tk::Comma => todo!(),
            Tk::Semi => todo!(),
            Tk::End => todo!(),
            Tk::String(_, _) => todo!(),
            Tk::Ident(_, _) => todo!(),
            Tk::Num(_, _) => todo!(),
            Tk::None => todo!(),
            Tk::Func => todo!(),
            Tk::Let => todo!(),
            Tk::If => todo!(),
            Tk::Else => todo!(),
            Tk::True => todo!(),
            Tk::False => todo!(),
            Tk::Return => "return",
            Tk::EQ => todo!(),
            Tk::NotEq => todo!(),
            Tk::Comment => todo!(),
            Tk::EMOJI4(_, _) => todo!(),
            Tk::EMOJI3(_, _) => todo!(),
            Tk::EMOJI2(_, _) => todo!(),
            Tk::EMOJI1(_, _) => todo!(),
        };
        Ok(())
    }
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
                    Tk::Assign
                }
            }
            b'!' => {
                if self.next_byte() == b'=' {
                    self.read_byte();
                    Tk::NotEq
                } else {
                    Tk::Bang
                }
            }
            b'/' => {
                if self.next_byte() == b'/' {
                    while self.next_byte() != b'\n' && self.next_byte() != 0 {
                        self.read_byte();
                    }
                    Tk::Comment
                } else {
                    Tk::Div
                }
            }
            b'+' => Tk::Plus,
            b'-' => Tk::Sub,
            b'*' => Tk::Mul,
            b'<' => Tk::LT,
            b'>' => Tk::GT,
            b';' => Tk::Semi,
            b'(' => Tk::LP,
            b')' => Tk::RP,
            b',' => Tk::Comma,
            b'{' => Tk::LB,
            b'}' => Tk::RB,
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
                Tk::String(x, self.current_line)
            }
            b'0'..=b'9' => {
                let start = self.current_position;
                while self.current_byte.is_ascii_digit() {
                    self.read_byte();
                }

                let vec = self.source_code[start..self.current_position].to_vec();
                // todo: mejorar esta vaina 👀
                let value = String::from_utf8(vec).unwrap();
                return Tk::Num(value, self.current_line);
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
                    "true" => Tk::True,
                    "false" => Tk::False,
                    "if" => Tk::If,
                    "else" => Tk::Else,
                    "return" => Tk::Return,
                    "let" => Tk::Let,
                    "fn" => Tk::Func,
                    _ => Tk::Ident(name, self.current_line),
                };
            }
            //todo: floats
            //todo: hex
            //todo: octal
            0 => Tk::End,
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
                    _ => Tk::None,
                }
            }
            //todo: handling errors: printf("Unrecognized character %c on line %d\n", c, Line);
            _ => Tk::None,
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
            Tk::Comment,
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
            Tk::String("Hello World!".to_string(), 1),
            // todo: deberíamos soportar 4-bytes emojis 👋🏾?
            Tk::EMOJI3("👋".to_owned(), 1),
            Tk::EMOJI3("🏾".to_owned(), 1),
            // todo: creo estamos dejando afuera el byte de variación
            Tk::EMOJI3("😍".to_owned(), 1),
            Tk::EMOJI3("🌍".to_owned(), 1),
            Tk::EMOJI3("🔥".to_owned(), 1),
            Tk::EMOJI2("✅".to_owned(), 1),
            Tk::EMOJI2("❌".to_owned(), 1),
            Tk::Semi,
            Tk::End,
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
            Tk::String("hola 😍🌍 mundo".to_string(), 1),
            Tk::Semi,
            Tk::End,
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
            (Tk::Assign, '='),
            (Tk::Plus, '+'),
            (Tk::LP, '('),
            (Tk::RP, ')'),
            (Tk::LB, '{'),
            (Tk::RB, '}'),
            (Tk::Comma, ','),
            (Tk::Semi, ';'),
            (Tk::End, '\0'),
        ];

        let mut lex = Scanner::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
            // println!("🎄{} - {}", String::from(tok), expected_literal.to_string());
            assert_eq!(String::from(&tok), expected_literal.to_string());
        }

        println!("✅ all seems good mf dance time💃🕺!");
    }

    #[test]
    fn it_reads_operators() {
        let input = "=+-!*/<>";
        let tests = [
            (Tk::Assign, '='),
            (Tk::Plus, '+'),
            (Tk::Sub, '-'),
            (Tk::Bang, '!'),
            (Tk::Mul, '*'),
            (Tk::Div, '/'),
            (Tk::LT, '<'),
            (Tk::GT, '>'),
        ];

        let mut lex = Scanner::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
            assert_eq!(String::from(&tok), expected_literal.to_string());
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
            (Tk::Func, "fn".to_string()),
            (Tk::Let, "let".to_string()),
            (Tk::If, "if".to_string()),
            (Tk::Else, "else".to_string()),
            (Tk::True, "true".to_string()),
            (Tk::False, "false".to_string()),
            (Tk::Return, "return".to_string()),
        ];

        let mut lex = Scanner::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
            assert_eq!(String::from(&tok), expected_literal.to_string());
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
            (Tk::NotEq, "!=".to_string()),
        ];

        let mut lex = Scanner::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
            assert_eq!(String::from(&tok), expected_literal.to_string());
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
            Tk::Let,
            Tk::Ident("five".to_string(), 2),
            Tk::Assign,
            Tk::Num("5".to_string(), 2),
            Tk::Semi,
            Tk::Let,
            Tk::Ident("ten".to_string(), 3),
            Tk::Assign,
            Tk::Num("10".to_string(), 3),
            Tk::Semi,
            Tk::Let,
            Tk::Ident("add".to_string(), 5),
            Tk::Assign,
            Tk::Func,
            Tk::LP,
            Tk::Ident("x".to_string(), 5),
            Tk::Comma,
            Tk::Ident("y".to_string(), 5),
            Tk::RP,
            Tk::LB,
            Tk::Ident("x".to_string(), 6),
            Tk::Plus,
            Tk::Ident("y".to_string(), 6),
            Tk::Semi,
            Tk::RB,
            Tk::Semi,
            Tk::Let,
            Tk::Ident("result".to_string(), 9),
            Tk::Assign,
            Tk::Ident("add".to_string(), 9),
            Tk::LP,
            Tk::Ident("five".to_string(), 9),
            Tk::Comma,
            Tk::Ident("ten".to_string(), 9),
            Tk::RP,
            Tk::Semi,
            Tk::End,
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
            Tk::Let,
            Tk::Ident("five".to_string(), 2),
            Tk::Assign,
            Tk::Num("5".to_string(), 2),
            Tk::Semi,
            Tk::Let,
            Tk::Ident("ten".to_string(), 3),
            Tk::Assign,
            Tk::Num("10".to_string(), 3),
            Tk::Semi,
            Tk::Bang,
            Tk::Sub,
            Tk::Div,
            Tk::Mul,
            Tk::Num("5".to_string(), 5),
            Tk::Semi,
            Tk::Num("5".to_string(), 6),
            Tk::LT,
            Tk::Num("10".to_string(), 6),
            Tk::GT,
            Tk::Num("5".to_string(), 6),
            Tk::Semi,
            Tk::End,
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
            Tk::Num("10".to_string(), 2),
            Tk::EQ,
            Tk::Num("10".to_string(), 2),
            Tk::Semi,
            Tk::Num("10".to_string(), 3),
            Tk::NotEq,
            Tk::Num("9".to_string(), 3),
            Tk::Semi,
            Tk::End,
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
            Tk::Num("10".to_string(), 2),
            Tk::EQ,
            Tk::Num("10".to_string(), 2),
            Tk::Semi,
            Tk::String("hola".to_string(), 3),
            Tk::Num("10".to_string(), 4),
            Tk::NotEq,
            Tk::Num("9".to_string(), 4),
            Tk::Semi,
            Tk::End,
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
            Tk::Num("10".to_string(), 2),
            Tk::EQ,
            Tk::Num("10".to_string(), 2),
            Tk::Semi,
            Tk::String("hola\n jamon".to_string(), 4),
            Tk::Num("10".to_string(), 5),
            Tk::NotEq,
            Tk::Num("9".to_string(), 5),
            Tk::Semi,
            Tk::End,
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
            Tk::Num("10".to_string(), 2),
            Tk::EQ,
            Tk::Num("10".to_string(), 2),
            Tk::Semi,
            Tk::String("hola".to_string(), 3),
            Tk::Comment,
            Tk::Num("10".to_string(), 5),
            Tk::NotEq,
            Tk::Num("9".to_string(), 5),
            Tk::Semi,
            Tk::End,
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
            Tk::Comment,
            Tk::Let,
            Tk::Ident("five".to_string(), 3),
            Tk::Assign,
            Tk::Num("5".to_string(), 3),
            Tk::Semi,
            Tk::Let,
            Tk::Ident("ten".to_string(), 4),
            Tk::Assign,
            Tk::Num("10".to_string(), 4),
            Tk::Semi,
            Tk::Bang,
            Tk::Sub,
            Tk::Div,
            Tk::Mul,
            Tk::Num("5".to_string(), 6),
            Tk::Semi,
            Tk::Num("5".to_string(), 7),
            Tk::LT,
            Tk::Num("10".to_string(), 7),
            Tk::GT,
            Tk::Num("5".to_string(), 7),
            Tk::Semi,
            Tk::If,
            Tk::LP,
            Tk::Num("5".to_string(), 9),
            Tk::LT,
            Tk::Num("10".to_string(), 9),
            Tk::RP,
            Tk::LB,
            Tk::Return,
            Tk::True,
            Tk::Semi,
            Tk::RB,
            Tk::Else,
            Tk::LB,
            Tk::Comment,
            Tk::Return,
            Tk::False,
            Tk::Semi,
            Tk::RB,
            Tk::Comment,
            Tk::End,
        ];

        let mut lex = Scanner::new(input.into());

        for expected_token in tests {
            let tok = lex.next_tok();
            assert_eq!(tok, expected_token);
        }
        println!("✅ all seems good mf dance time💃🕺!");
    }
}
