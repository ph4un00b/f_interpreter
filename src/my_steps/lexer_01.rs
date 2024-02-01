// valor máximo
// todo: https://doc.rust-lang.org/nomicon/repr-rust.html
// todo: https://doc.rust-lang.org/nomicon/other-reprs.html
//? #[repr(u8)]
//? enum Token{
//?   Assing = (Decimal del char)
//? .....
//? }
// todo: #[derive(PartialEq, Eq, Partial, Ord, PartialOrd, Debug, Clone)]
//? @see: https://doc.rust-lang.org/std/ops/index.html
#[derive(PartialEq, Debug)]
pub enum TokenType {
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

//? 👀 usize ❓: is an architecture-dependent type whose size is determined by
//? the platform's memory addressing capability. On a 64-bit platform,
//? for example, usize is 64 bits (8 bytes), while on a 32-bit platform,
//? it is 32 bits (4 bytes)

//? In general, you should use usize when dealing with memory-related operations,
//? such as indexing into arrays or calculating buffer sizes,
//? since it ensures that the indices or sizes can address all the memory available
//? on that particular platform.

//? use u32 on fixed bit values
type Byte = u8;
pub struct Lexer {
    //? trying vector of bytes, easier mode ❓
    //? maybe not for emojis later on
    //? input: String,
    input: Vec<Byte>,
    // todo: hay una forma de crear defaults!
    current_pos: usize,
    next_pos: usize,
    //? ch: char
    ch: Byte,
}

impl Lexer {
    // todo: checar por que no funka el formatter
    // pub fn new(input: impl Into<String>) -> Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Self {
            input: input.into_bytes(),
            current_pos: 0,
            next_pos: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    pub fn next_tok(&mut self) -> Token {
        let tok = match self.ch {
            //?  tabla https://www.rapidtables.com/code/text/ascii-table.html
            b'=' => new_token(TokenType::ASSIGN, self.ch),
            b';' => new_token(TokenType::SEMICOLON, self.ch),
            b'(' => new_token(TokenType::LPAREN, self.ch),
            b')' => new_token(TokenType::RPAREN, self.ch),
            b',' => new_token(TokenType::COMMA, self.ch),
            b'+' => new_token(TokenType::PLUS, self.ch),
            b'{' => new_token(TokenType::LBRACE, self.ch),
            b'}' => new_token(TokenType::RBRACE, self.ch),
            0 => new_token(TokenType::EOF, 0),
            //? quizá truene
            _ => todo!("😎😋😊😘"),
        };
        self.read_char();

        tok
    }

    //? only supports ASCII characters instead of the full Unicode range
    //? le da flojera suportar emojis ❌
    fn read_char(&mut self) {
        // todo: hacer esto más rústico😁
        if self.next_pos >= self.input.len() {
            //? the ASCII code for the "NUL" character and signifies
            //? either “we haven’t read anything yet”
            //? or “end of file” for us
            self.ch = 0;
        } else {
            //? creo de esta forma se soporta emojis más sencillo 😎
            // todo: emojis: por ahora lo manejamos en full ASCII
            let char = self.input[self.next_pos];
            self.ch = char;
        }
        self.current_pos = self.next_pos;
        self.next_pos += 1;
    }
}

// todo: struct de control, refactorizar a traits de casteo ❓👀
pub struct Token {
    pub kind: TokenType,
    pub literal: Byte,
}

fn new_token(token_type: TokenType, ch: Byte) -> Token {
    // todo: https://doc.rust-lang.org/std/cmp/index.html
    // todo: osea el refactor aquí fue preferir los traits de
    // todo: casteo sobre el struct de control
    //? fn some_function<Into<TokenType o char>>(.....) -> ..
    //? Token { ch , kind} -> props ...
    //? TokenType o algo que podes castear a ese value
    Token {
        kind: token_type,
        literal: ch,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "=+(),{};";

        let tests = [
            (TokenType::ASSIGN, b'='),
            (TokenType::PLUS, b'+'),
            (TokenType::LPAREN, b'('),
            (TokenType::RPAREN, b')'),
            (TokenType::COMMA, b','),
            (TokenType::LBRACE, b'{'),
            (TokenType::RBRACE, b'}'),
            (TokenType::SEMICOLON, b';'),
            (TokenType::EOF, b'\0'),
        ];

        let mut lex = Lexer::new(input.into());

        for (expected_token, expected_literal) in tests {
            let tok = lex.next_tok();
            assert_eq!(tok.kind, expected_token);
            // println!("💃 {:?}, expected {:?}", tok.kind, expected_token);
            // println!("🕺 {:?}, expected {:?}", tok.literal, expected_literal);
            assert_eq!(tok.literal, expected_literal);
        }
    }
}
