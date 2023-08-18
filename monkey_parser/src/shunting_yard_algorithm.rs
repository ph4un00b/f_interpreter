use crate::{lexer::Lexer, scanner::Tk};
#[allow(unused)]
enum ParserErr {
    BadExpression(Tk),
    ParentMismatch,
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Sum,
    Product,
}
impl From<&Tk> for Precedence {
    fn from(value: &Tk) -> Precedence {
        match value {
            Tk::Plus => Precedence::Sum,
            Tk::Sub => Precedence::Sum,
            Tk::Div => Precedence::Product,
            Tk::Mul => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

impl From<&Op> for Precedence {
    fn from(value: &Op) -> Precedence {
        match value {
            Op::Add => Precedence::Sum,
            Op::Sub => Precedence::Sum,
            Op::Mul => Precedence::Product,
            Op::Div => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

impl From<&Tk> for Op {
    fn from(value: &Tk) -> Op {
        match value {
            Tk::Plus => Op::Add,
            Tk::Sub => Op::Sub,
            Tk::Div => Op::Div,
            Tk::Mul => Op::Mul,
            _ => unreachable!(),
        }
    }
}
#[allow(unused)]
#[derive(PartialEq, PartialOrd, Clone, Debug)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    OpenParen,
    CloseParen,
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Add => write!(f, "+")?,
            Op::Sub => write!(f, "-")?,
            Op::Mul => write!(f, "*")?,
            Op::Div => write!(f, "/")?,
            Op::OpenParen => write!(f, "(")?,
            Op::CloseParen => write!(f, ")")?,
        }
        Ok(())
    }
}
#[derive(Debug, Clone)]
enum Exp {
    Literal(i64),
    Op(Op),
}

impl std::fmt::Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Exp::Literal(val) => write!(f, "{val}")?,
            Exp::Op(op) => write!(f, "{op}")?,
        }
        Ok(())
    }
}
use std::{collections::VecDeque, result::Result as StdResult};
type Result<T> = StdResult<T, ParserErr>;

#[derive(Debug)]
struct ParserCalc {
    lex: Lexer,
    current_token: Tk,
    peek_token: Tk,
    west: Vec<Exp>,
    south: VecDeque<Op>,
}

#[allow(unused)]
impl ParserCalc {
    pub fn new(lex: Lexer) -> Self {
        let mut p = ParserCalc {
            lex,
            current_token: Tk::None,
            peek_token: Tk::None,
            west: vec![],
            south: VecDeque::new(),
        };
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        if let Some(next_token) = self.lex.next() {
            self.peek_token = next_token;
        } else {
            self.peek_token = Tk::None;
        };
    }

    fn parse_program(&mut self) -> Vec<String> {
        while self.current_token != Tk::End {
            if let Err(e) = self.expression() {
                match e {
                    ParserErr::BadExpression(tk) => panic!("bad {tk}"),
                    ParserErr::ParentMismatch => panic!("parenthesis mismatch"),
                }
            };
            self.next_token();
        }
        while let Some(top) = self.south.pop_back() {
            self.west.push(Exp::Op(top.clone()));
        }

        self.west
            .clone()
            .into_iter()
            .map(|v| v.to_string())
            .collect()
    }

    fn expression(&mut self) -> Result<()> {
        println!("{:?}", self.south.clone());
        let token = self.current_token.to_owned();
        match &self.current_token {
            Tk::Num(val, _) => {
                if let Ok(num) = val.parse::<i64>() {
                    self.west.push(Exp::Literal(num))
                } else {
                    return Err(ParserErr::BadExpression(token));
                };
            }
            Tk::Comma => {
                while let Some(top) = self.south.pop_back() {
                    self.west.push(Exp::Op(top.clone()));
                }
            }
            Tk::OpenParen => self.south.push_back(Op::OpenParen),
            Tk::CloseParen => {
                while let Some(top) = self.south.pop_back() {
                    if top == Op::OpenParen {
                        self.south.pop_back();
                        break;
                    } else {
                        self.west.push(Exp::Op(top.clone()));
                    }
                }
            }
            Tk::Plus | Tk::Sub | Tk::Mul | Tk::Div => {
                while let Some(top) = self.south.pop_back() {
                    if Precedence::from(&top) >= Precedence::from(&self.current_token) {
                        self.west.push(Exp::Op(top.clone()));
                    } else {
                        break;
                    }
                }
                self.south.push_back(Op::from(&self.current_token))
            }
            _ => (),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::ParserCalc;
    use crate::lexer::Lexer;

    #[test]
    fn test_parenthesis() {
        let lex = Lexer::new("5 * (2 - 3)".into());
        let mut p = ParserCalc::new(lex);
        let infix = p.parse_program();
        println!("{:?}", p.west);
        assert_eq!(infix, vec!["5", "2", "3", "-", "*"])
    }

    #[test]
    fn test_precedence() {
        let lex = Lexer::new("5 * 2 - 3".into());
        let mut p = ParserCalc::new(lex);
        let infix = p.parse_program();
        println!("{:?}", p.west);
        assert_eq!(infix, vec!["5", "2", "*", "3", "-"])
    }

    #[test]
    fn test_simple() {
        let lex = Lexer::new("2 + 1".into());
        let mut p = ParserCalc::new(lex);
        let infix = p.parse_program();
        println!("{:?}", p.west);
        assert_eq!(infix, vec!["2", "1", "+"])
    }
}
