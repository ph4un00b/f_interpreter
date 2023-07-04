use std::fmt;

use crate::pratt_lexer_01::{Lexer, Tk};

#[derive(PartialEq, Debug, Clone)]
enum Parselet {
    ID,
    UNARY,
    // POSTFIX,
    BINARY,
    // BRANCH,
}

#[derive(PartialEq, Debug, Clone)]
enum Expression {
    NAME {
        kind: Parselet,
        token: Tk,
    },
    PREOP {
        kind: Parselet,
        operator: Tk,
        operand: Box<Expression>,
    },
    OPERATOR {
        kind: Parselet,
        left: Box<Expression>,
        operator: Tk,
        right: Box<Expression>,
    },
    // POSTOP {
    //     kind: Parselet,
    //     left: Box<Expression>,
    //     token: Tk,
    // },
    // BRANCH {
    //     kind: Parselet,
    //     left: Box<Expression>,
    //     then: Box<Expression>,
    //     otherwise: Box<Expression>,
    // },
    // // * for testing
    // NONE,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::NAME { kind, token } => {
                write!(f, "{}", String::from(token.clone()))?;
            }
            Expression::PREOP {
                kind,
                operator,
                operand,
            } => {
                write!(
                    f,
                    "({}{})",
                    String::from(operator.clone()),
                    operand.to_string()
                )?;
            }
            Expression::OPERATOR {
                kind,
                left,
                operator,
                right,
            } => {
                write!(
                    f,
                    "({} {} {})",
                    left.to_string(),
                    String::from(operator.clone()),
                    right.to_string()
                )?;
            }
        };

        Ok(())
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
enum Precedence {
    //? _ int = iota
    //? LOWEST
    //? EQUALS // ==
    //? LESSGREATER // > or <
    //? SUM // +
    //? PRODUCT // *
    //? PREFIX // -X or !X
    //? CALL // myFunction(X)
    DEFAULT,
    SUM,
    PRODUCT,
    PREFIX,
}

impl From<Tk> for Precedence {
    fn from(value: Tk) -> Precedence {
        match value {
            Tk::PLUS(_, _) => Precedence::SUM,
            Tk::MINUS(_, _) => Precedence::SUM,
            Tk::ASTERISK(_, _) => Precedence::PRODUCT,
            Tk::SLASH(_, _) => Precedence::PRODUCT,
            _ => Precedence::DEFAULT,
        }
    }
}

trait Pratt {
    fn parse_next_expression(&mut self, precedence: Precedence) -> Expression;
    fn consume_token(&mut self);
    fn lookahead_by(&self, distance: usize);
}

#[derive(Clone)]
struct Parser {
    tokens: Lexer,
    _errors: Vec<String>,
    _consumed: Vec<Tk>,
    current_token: Tk,
    next_token: Tk,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            tokens: lexer,
            _errors: vec![],
            _consumed: vec![],
            current_token: Tk::ILEGAL,
            next_token: Tk::ILEGAL,
        };

        p.consume_token();
        p.consume_token();

        p
    }

    fn parse_prefix_exp(&mut self) -> Expression {
        let name_token = self.current_token.clone();
        self.consume_token();

        // * unary expression at least have one operand
        Expression::PREOP {
            kind: Parselet::UNARY,
            operator: name_token,
            operand: Box::new(self.parse_next_expression(Precedence::PREFIX)),
        }
    }

    fn parse_id_exp(&mut self) -> Expression {
        //? id parselet should be considered unary❓
        Expression::NAME {
            kind: Parselet::ID,
            token: self.current_token.clone(),
        }
    }

    fn parse_infix_exp(&mut self, left: &Expression) -> Expression {
        // * It’s important to note that left is our already parsed expression
        // * saves the precedence, advances the
        // * tokens and calls #parse_next_expression
        // * - passing in the just saved precedence.
        let name_token = self.current_token.clone();

        let current_precedence = Precedence::from(self.current_token.clone());
        self.consume_token();

        Expression::OPERATOR {
            kind: Parselet::BINARY,
            left: Box::new(left.clone()),
            operator: name_token,
            right: Box::new(self.parse_next_expression(current_precedence)),
        }
    }
}

impl Pratt for Parser {
    fn parse_next_expression(&mut self, precedence: Precedence) -> Expression {
        println!("parsing - {:?} - pre {:?}", self.current_token, precedence);

        // * null denotations aka nuds😏
        let mut left_expr = match self.current_token {
            Tk::NAME(_, _) => self.parse_id_exp(),
            Tk::MINUS(_, _) => self.parse_prefix_exp(),
            Tk::BANG(_, _) => self.parse_prefix_exp(),
            _ => panic!("❌illegal prefix expression {:?}", self.current_token),
        };

        while precedence < Precedence::from(self.next_token.clone()) {
            // * left denotations aka leds
            match self.next_token {
                Tk::PLUS(_, _) => true,
                Tk::MINUS(_, _) => true,
                Tk::ASTERISK(_, _) => true,
                Tk::SLASH(_, _) => true,
                _ => return left_expr,
            };

            self.consume_token();

            left_expr = match self.current_token {
                // * infixes
                Tk::PLUS(_, _) => self.parse_infix_exp(&left_expr),
                Tk::MINUS(_, _) => self.parse_infix_exp(&left_expr),
                Tk::ASTERISK(_, _) => self.parse_infix_exp(&left_expr),
                Tk::SLASH(_, _) => self.parse_infix_exp(&left_expr),
                _ => panic!("no debe llegar aquí!❌"),
            }
        }

        left_expr
    }

    fn consume_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = self.tokens.next().unwrap_or(Tk::ILEGAL);
        // println!("consumed {:?}", self.current_token);
    }

    fn lookahead_by(&self, _distance: usize) {
        // * quizá esta funcionalidad será remplazada por
        // * self#next_token, self#current_token, self#next_token
        // * si obtenemos el tamaño del iterador, se va consumir potencialmente e innecesariamente ❗❓
        // * en otras implementaciones se asume que ya están en memoria los tokens
        // * @see https://github.com/Mountagha/bantam/blob/main/parser.py
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "-a";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);

        let parsed = parser.parse_next_expression(Precedence::DEFAULT);
        assert_eq!(
            parsed,
            Expression::PREOP {
                kind: Parselet::UNARY,
                operator: Tk::MINUS("-".to_string(), 1),
                operand: Box::new(Expression::NAME {
                    kind: Parselet::ID,
                    token: Tk::NAME("a".to_string(), 1)
                })
            }
        );
        assert_eq!(parsed.to_string(), "(-a)".to_string());
    }

    #[test]
    fn it_works_with_infixes() {
        let unary_ops = [("a + b", Tk::ILEGAL)];

        for (input, _expected_token) in unary_ops {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression(Precedence::DEFAULT);
            assert_eq!(
                parsed,
                Expression::OPERATOR {
                    kind: Parselet::BINARY,
                    left: Box::new(Expression::NAME {
                        kind: Parselet::ID,
                        token: Tk::NAME("a".to_string(), 1)
                    }),
                    operator: Tk::PLUS("+".to_string(), 1),
                    right: Box::new(Expression::NAME {
                        kind: Parselet::ID,
                        token: Tk::NAME("b".to_string(), 1)
                    })
                },
            );
            assert_eq!(parsed.to_string(), "(a + b)".to_string());
        }
    }

    #[test]
    fn it_works_with_associative_left() {
        let unary_ops = [("a + b + c", Tk::ILEGAL)];

        for (input, _expected_token) in unary_ops {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression(Precedence::DEFAULT);
            assert_eq!(
                parsed,
                // * without precedence
                // * a - (b - c)
                // * right associative by default
                // * expected -> (a - b) - c
                Expression::OPERATOR {
                    kind: Parselet::BINARY,
                    left: Box::new(Expression::OPERATOR {
                        kind: Parselet::BINARY,
                        left: Box::new(Expression::NAME {
                            kind: Parselet::ID,
                            token: Tk::NAME("a".to_string(), 1)
                        }),
                        operator: Tk::PLUS("+".to_string(), 1),
                        right: Box::new(Expression::NAME {
                            kind: Parselet::ID,
                            token: Tk::NAME("b".to_string(), 1)
                        })
                    }),
                    operator: Tk::PLUS("+".to_string(), 1),
                    right: Box::new(Expression::NAME {
                        kind: Parselet::ID,
                        token: Tk::NAME("c".to_string(), 1)
                    })
                },
            );
            assert_eq!(parsed.to_string(), "((a + b) + c)".to_string());
        }
    }

    #[test]
    fn it_works_with_debug_infixes() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression(Precedence::DEFAULT);

            assert_eq!(parsed.to_string(), expected.to_string());
        }
    }
}
