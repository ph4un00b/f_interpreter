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
    LPAREN(String, i32),
    RPAREN(String, i32),
    COMMA(String, i32),
    ASSIGN(String, i32),
    PLUS(String, i32),
    MINUS(String, i32),
    ASTERISK(String, i32),
    SLASH(String, i32),
    CARET(String, i32),
    TILDE(String, i32),
    BANG(String, i32),
    QUESTION(String, i32),
    COLON(String, i32),
    NAME(String, i32),
    EOF(String, i32),
    ILEGAL(String, i32),
}

#[derive(Debug)]
pub struct Lexer {
    pub had_error: bool,
    pub had_runtime_error: bool,
    scanner: Scanner,
}

impl Iterator for Lexer {
    type Item = <Scanner as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        <Scanner as Iterator>::next(&mut self.scanner)
    }
}

impl Lexer {
    pub fn new(source_code: String) -> Self {
        Self {
            scanner: Scanner::new(source_code),
            had_error: false,
            had_runtime_error: false,
        }
    }
}

pub type Byte = u8;

#[derive(Debug)]
pub struct Scanner {
    source_code: Vec<Byte>,
    tokens: Vec<Tk>,
    current_position: usize,
    next_position: usize,
    current_byte: Byte,
    current_line: i32,
}

impl Iterator for Scanner {
    type Item = Tk;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_position > self.source_code.len() {
            None
        } else {
            let tk = self.next_tok();
            Some(tk)
        }
    }
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
            current_line: 1,
            tokens: Vec::new(),
        };
        l.consume_byte();
        l
    }

    fn consume_byte(&mut self) {
        self.current_byte = if self.next_position >= self.source_code.len() {
            0
        } else {
            self.source_code[self.next_position]
        };
        self.current_position = self.next_position;
        self.next_position += 1;
    }

    /*
     * last 2 empties chars are TokenType.NAME & TokenType.EOF respectively
     *
     * string_values
     * = ['(', ')', ',', '=', '+', '-', '*', '/', '^', '~', '!', '?', ':', '', '']
     */
    fn next_tok(&mut self) -> Tk {
        while self.current_byte.is_ascii_whitespace() {
            if let b'\n' = self.current_byte {
                self.current_line += 1;
            }
            self.consume_byte();
        }

        let tok = match self.current_byte {
            //? operators
            b'=' => Tk::ASSIGN("=".to_string(), self.current_line),
            b'!' => Tk::BANG("!".to_string(), self.current_line),
            b'/' => Tk::SLASH("/".to_string(), self.current_line),
            b'+' => Tk::PLUS("+".to_string(), self.current_line),
            b'-' => Tk::MINUS("-".to_string(), self.current_line),
            b'*' => Tk::ASTERISK("*".to_string(), self.current_line),
            b')' => Tk::RPAREN(")".to_string(), self.current_line),
            b'(' => Tk::LPAREN("(".to_string(), self.current_line),
            b',' => Tk::COMMA(",".to_string(), self.current_line),
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
                let start = self.current_position;
                while self.current_byte.is_ascii_alphabetic() {
                    self.consume_byte();
                }
                let vec = self.source_code[start..self.current_position].to_vec();
                let name = String::from_utf8(vec).unwrap();

                Tk::NAME(name, self.current_line)
            }
            0 => Tk::EOF("0".to_string(), self.current_line),
            _ => Tk::ILEGAL("_".to_string(), self.current_line),
        };

        self.consume_byte();
        tok
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "=+(),";

        let tests = [
            Tk::ASSIGN("=".to_string(), 1),
            Tk::PLUS("+".to_string(), 1),
            Tk::LPAREN("(".to_string(), 1),
            Tk::RPAREN(")".to_string(), 1),
            Tk::COMMA(",".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }
}
