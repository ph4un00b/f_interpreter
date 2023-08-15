use crate::scanner::{Byte, Scanner, Tk};

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
            b'=' => Ok(Tk::Assign),
            b';' => Ok(Tk::Semi),
            b'(' => Ok(Tk::OpenParen),
            b')' => Ok(Tk::CloseParen),
            b',' => Ok(Tk::Comma),
            b'+' => Ok(Tk::Plus),
            b'{' => Ok(Tk::LB),
            b'}' => Ok(Tk::RB),
            0 => Ok(Tk::End),
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
impl From<&Tk> for String {
    fn from(value: &Tk) -> String {
        match value {
            Tk::Assign => "=".to_string(),
            Tk::Semi => ";".to_string(),
            Tk::OpenParen => "(".to_string(),
            Tk::CloseParen => ")".to_string(),
            Tk::Comma => ",".to_string(),
            Tk::Plus => "+".to_string(),
            Tk::LB => "{".to_string(),
            Tk::RB => "}".to_string(),
            Tk::End => "\0".to_string(),
            //? operators
            Tk::Sub => "-".to_string(),
            Tk::Bang => "!".to_string(),
            Tk::Mul => "*".to_string(),
            Tk::Div => "/".to_string(),
            Tk::LT => "<".to_string(),
            Tk::GT => ">".to_string(),
            //? keywords
            Tk::Func => "fn".to_string(),
            Tk::Let => "let".to_string(),
            Tk::If => "if".to_string(),
            Tk::Else => "else".to_string(),
            Tk::True => "true".to_string(),
            Tk::False => "false".to_string(),
            Tk::Return => "return".to_string(),
            //? two-character
            Tk::EQ => "==".to_string(),
            Tk::NotEq => "!=".to_string(),
            //? special
            Tk::Ident(name, _) => name.clone(),
            Tk::Num(value, _) => value.clone(),
            Tk::None => todo!(),
            Tk::String(_, _) => todo!(),
            Tk::Comment => todo!(),
            Tk::EMOJI4(_, _) => todo!(),
            Tk::EMOJI3(_, _) => todo!(),
            Tk::EMOJI2(_, _) => todo!(),
            Tk::EMOJI1(_, _) => todo!(),
        }
    }
}

impl std::fmt::Display for Tk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tk::Assign
            | Tk::Plus
            | Tk::Sub
            | Tk::Bang
            | Tk::Mul
            | Tk::Div
            | Tk::LT
            | Tk::GT
            | Tk::OpenParen
            | Tk::CloseParen
            | Tk::LB
            | Tk::RB
            | Tk::Comma
            | Tk::Semi
            | Tk::End
            | Tk::String(_, _)
            | Tk::Ident(_, _)
            | Tk::Num(_, _)
            | Tk::None
            | Tk::Func
            | Tk::Let
            | Tk::If
            | Tk::Else
            | Tk::True
            | Tk::False
            | Tk::Return
            | Tk::EQ
            | Tk::NotEq
            | Tk::Comment
            | Tk::EMOJI4(_, _)
            | Tk::EMOJI3(_, _)
            | Tk::EMOJI2(_, _)
            | Tk::EMOJI1(_, _) => write!(f, "{}", String::from(self))?,
        };
        Ok(())
    }
}

#[derive(Debug)]
pub struct Lexer {
    pub had_error: bool,
    pub had_runtime_error: bool,
    //? esta propiedad deberia borrarse, para no repetir en memoria
    tokens: Vec<Tk>,
    //? para el iterador
    pub current_tk: usize,
}

impl Iterator for Lexer {
    type Item = Tk;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.is_empty() || self.current_tk >= self.tokens.len() {
            None
        } else {
            let tk = self.tokens[self.current_tk].clone();
            // println!("tok {:?} - {:?}", self.current_tk, tk);
            self.current_tk += 1;
            Some(tk)
        }
    }
}

#[allow(unused)]
impl Lexer {
    pub fn new(source_code: String) -> Self {
        let mut scanner = Scanner::new(source_code);
        Self {
            had_error: false,
            had_runtime_error: false,
            //? duplicando en la memoria 👎
            tokens: scanner.scan_tokens().clone(),
            current_tk: 0,
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
            Tk::Assign,
            Tk::Plus,
            Tk::OpenParen,
            Tk::CloseParen,
            Tk::LB,
            Tk::RB,
            Tk::Comma,
            Tk::Semi,
            Tk::End,
        ];

        let lex = Lexer::new(input.into());
        assert_eq!(lex.tokens, expected_tokens);
    }
}
