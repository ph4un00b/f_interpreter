use crate::scanner_03_emoji_as_token::{Byte, Scanner, Tk};

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
            Tk::IDENT(_, _) => todo!(),
            Tk::INT(_, _) => todo!(),
            Tk::ILEGAL => todo!(),
            Tk::STR(_, _) => todo!(),
            Tk::COMMENT => todo!(),
            Tk::EMOJI(_, _) => todo!(),
            Tk::INVALIDEMOJI(_, _) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    pub had_error: bool,
    pub had_runtime_error: bool,
    //? esta propiedad deberia borrarse, para no repetir en memoria
    tokens: Vec<Tk>,
}

impl Lexer {
    pub fn new(source_code: String) -> Self {
        let mut scanner = Scanner::new(source_code);
        Self {
            had_error: false,
            had_runtime_error: false,
            //? duplicando en la memoria 👎
            tokens: scanner.scan_tokens().clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        let lex = Lexer::new(input.into());
        assert_eq!(lex.tokens, expected_tokens);
    }
}
