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

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
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
    LTHAN(String, i32),
    COLON(String, i32),
    NAME(String, i32),
    EOF(String, i32),
    // ILEGAL(String, i32),
    ILEGAL,
    TRUE(String, i32),
    FALSE(String, i32),
}

impl From<Tk> for String {
    fn from(value: Tk) -> Self {
        match value {
            Tk::ASSIGN(_, _) => "=".to_string(),
            Tk::LPAREN(_, _) => "(".to_string(),
            Tk::RPAREN(_, _) => ")".to_string(),
            Tk::COMMA(_, _) => ",".to_string(),
            Tk::PLUS(_, _) => "+".to_string(),
            Tk::EOF(_, _) => "\0".to_string(),
            Tk::MINUS(_, _) => "-".to_string(),
            Tk::BANG(_, _) => "!".to_string(),
            Tk::ASTERISK(_, _) => "*".to_string(),
            Tk::SLASH(_, _) => "/".to_string(),
            Tk::CARET(_, _) => "^".to_string(),
            Tk::TILDE(_, _) => "~".to_string(),
            Tk::QUESTION(_, _) => "?".to_string(),
            Tk::COLON(_, _) => ":".to_string(),
            Tk::NAME(name, _) => name,
            Tk::ILEGAL => "_".to_string(),
            Tk::LTHAN(_, _) => "<".to_string(),
            Tk::TRUE(value, _) => value,
            Tk::FALSE(value, _) => value,
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

        // let vec = vec![self.current_byte];
        // let name = String::from_utf8(vec).unwrap();
        // println!("=> ch: {}", name);

        self.current_position = self.next_position;
        self.next_position += 1;
    }

    fn next_tok(&mut self) -> Tk {
        while self.current_byte.is_ascii_whitespace() {
            if let b'\n' = self.current_byte {
                self.current_line += 1;
            }
            self.consume_byte();
        }

        let vec = [self.current_byte].to_vec();
        // todo: mejorar esta vaina 👀
        let name = String::from_utf8(vec).unwrap();
        println!("current {:?}", name);

        let tok = match self.current_byte {
            //? operators
            b'<' => Tk::LTHAN("<".to_string(), self.current_line),
            b'?' => Tk::QUESTION("?".to_string(), self.current_line),
            b':' => Tk::COLON(":".to_string(), self.current_line),
            b'=' => Tk::ASSIGN("=".to_string(), self.current_line),
            b'!' => Tk::BANG("!".to_string(), self.current_line),
            b'/' => Tk::SLASH("/".to_string(), self.current_line),
            b'+' => Tk::PLUS("+".to_string(), self.current_line),
            b'-' => Tk::MINUS("-".to_string(), self.current_line),
            b'*' => Tk::ASTERISK("*".to_string(), self.current_line),
            b'(' => Tk::LPAREN("(".to_string(), self.current_line),
            b')' => Tk::RPAREN(")".to_string(), self.current_line),
            b',' => Tk::COMMA(",".to_string(), self.current_line),
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
                let start = self.current_position;
                /*
                 * usage:
                 * allow snake_case
                 * allow ? for query methods f.i.: jamon?
                 * allow ! for throwing methods f.i.: jamon!
                 */
                while self.current_byte.is_ascii_alphabetic() || self.current_byte == b'_' {
                    self.consume_byte();
                }
                let vec = self.source_code[start..self.current_position].to_vec();
                // todo: mejorar esta vaina 👀
                let name = String::from_utf8(vec).unwrap();

                return match name.as_str() {
                    "true" => Tk::TRUE(name, self.current_line),
                    "false" => Tk::FALSE(name, self.current_line),
                    _ => Tk::NAME(name, self.current_line),
                };
            }
            0 => Tk::EOF("0".to_string(), self.current_line),
            // _ => Tk::ILEGAL("_".to_string(), self.current_line),
            _ => Tk::ILEGAL,
        };

        self.consume_byte();
        // println!("tk: {:?}", tok);
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

/*
 * @see http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
 */
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "from + offset(time)";

        let tests = [
            Tk::NAME("from".to_string(), 1),
            Tk::PLUS("+".to_string(), 1),
            Tk::NAME("offset".to_string(), 1),
            Tk::LPAREN("(".to_string(), 1),
            Tk::NAME("time".to_string(), 1),
            Tk::RPAREN(")".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }

    #[test]
    fn it_works_2() {
        let input = "-a + b";

        let tests = [
            Tk::MINUS("-".to_string(), 1),
            Tk::NAME("a".to_string(), 1),
            Tk::PLUS("+".to_string(), 1),
            Tk::NAME("b".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }

    #[test]
    fn it_works_3() {
        let input = "a!";

        let tests = [
            Tk::NAME("a".to_string(), 1),
            Tk::BANG("!".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }

    #[test]
    fn it_works_4() {
        let input = "b ? c! : -d";

        let tests = [
            Tk::NAME("b".to_string(), 1),
            Tk::QUESTION("?".to_string(), 1),
            Tk::NAME("c".to_string(), 1),
            Tk::BANG("!".to_string(), 1),
            Tk::COLON(":".to_string(), 1),
            Tk::MINUS("-".to_string(), 1),
            Tk::NAME("d".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }

    #[test]
    fn it_works_5() {
        let input = "a < b < c";

        let tests = [
            Tk::NAME("a".to_string(), 1),
            Tk::LTHAN("<".to_string(), 1),
            Tk::NAME("b".to_string(), 1),
            Tk::LTHAN("<".to_string(), 1),
            Tk::NAME("c".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }

    #[test]
    fn it_works_6() {
        let input = "foobar = true";

        let tests = [
            Tk::NAME("foobar".to_string(), 1),
            Tk::ASSIGN("=".to_string(), 1),
            Tk::TRUE("true".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }

    #[test]
    fn it_works_7() {
        let input = "a < b + false";

        let tests = [
            Tk::NAME("a".to_string(), 1),
            Tk::LTHAN("<".to_string(), 1),
            Tk::NAME("b".to_string(), 1),
            Tk::PLUS("+".to_string(), 1),
            Tk::FALSE("false".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }

    #[test]
    fn it_works_8() {
        let input = "a + (b + c) + d";

        let tests = [
            Tk::NAME("a".to_string(), 1),
            Tk::PLUS("+".to_string(), 1),
            Tk::LPAREN("(".to_string(), 1),
            Tk::NAME("b".to_string(), 1),
            Tk::PLUS("+".to_string(), 1),
            Tk::NAME("c".to_string(), 1),
            Tk::RPAREN(")".to_string(), 1),
            Tk::PLUS("+".to_string(), 1),
            Tk::NAME("d".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }

    #[test]
    fn it_works_9() {
        let input = "!(a = a)";

        let tests = [
            Tk::BANG("!".to_string(), 1),
            Tk::LPAREN("(".to_string(), 1),
            Tk::NAME("a".to_string(), 1),
            Tk::ASSIGN("=".to_string(), 1),
            Tk::NAME("a".to_string(), 1),
            Tk::RPAREN(")".to_string(), 1),
            Tk::EOF("0".to_string(), 1),
        ];

        let mut lex = Lexer::new(input.into());

        for expected_token in tests.iter() {
            // todo: #clone necesario❓
            assert_eq!(lex.next(), Some(expected_token.clone()));
        }
    }
}
