use std::{collections::HashMap, usize};

use crate::pratt_lexer_01::{Lexer, Tk};

#[derive(PartialEq, Debug, Clone)]
enum Parselet {
    NAME,
    PREFIX,
    POSTFIX,
    BINARY,
    COND,
}

#[derive(PartialEq, Debug, Clone)]
enum Expression {
    NAME {
        kind: Parselet,
        token: Tk,
    },
    PREOP {
        kind: Parselet,
        token: Tk,
        operand: Box<Expression>,
    },
    OPERATOR {
        kind: Parselet,
        left: Box<Expression>,
        token: Tk,
        right: Box<Expression>,
    },
    POSTOP {
        kind: Parselet,
        left: Box<Expression>,
        token: Tk,
    },
    BRANCH {
        kind: Parselet,
        left: Box<Expression>,
        then: Box<Expression>,
        otherwise: Box<Expression>,
    },
    // * for testing
    NONE,
}

trait Pratt {
    fn parse_next_expression(&mut self) -> Expression;
    fn consume_token(&mut self);
    fn lookahead_by(&self, distance: usize);
    fn parse_prefix(&self);
    fn parse_infix(&self);
}

#[derive(Clone)]
struct Parser {
    tokens: Lexer,
    errors: Vec<String>,
    consumed: Vec<Tk>,
    current_token: Tk,
    next_token: Tk,
    prefixes: HashMap<Tk, Parselet>,
    infixes: HashMap<Tk, Parselet>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            tokens: lexer,
            errors: vec![],
            consumed: vec![],
            current_token: Tk::ILEGAL,
            next_token: Tk::ILEGAL,
            prefixes: HashMap::new(),
            infixes: HashMap::new(),
        };

        p.consume_token();
        p.consume_token();
        //* GRAMMAR
        //? p.prefixes
        //?     .insert(Tk::NAME("".to_string(), 0), Parselet::NAME);
        //? p.prefixes
        //?     .insert(Tk::PLUS("".to_string(), 0), Parselet::PREFIX);
        //? p.prefixes
        //?     .insert(Tk::MINUS("".to_string(), 0), Parselet::PREFIX);
        //? p.prefixes
        //?     .insert(Tk::TILDE("".to_string(), 0), Parselet::PREFIX);
        //? p.prefixes
        //?     .insert(Tk::BANG("".to_string(), 0), Parselet::PREFIX);

        p
    }
}

impl Pratt for Parser {
    fn parse_next_expression(&mut self) -> Expression {
        println!("tok - {:?}", self.current_token);

        let left = match self.current_token {
            Tk::NAME(_, _) => Expression::NAME {
                token: self.current_token.clone(),
                kind: Parselet::PREFIX,
            },
            Tk::PLUS(_, _) | Tk::MINUS(_, _) | Tk::TILDE(_, _) | Tk::BANG(_, _) => {
                let name_token = self.current_token.clone();
                self.consume_token();

                Expression::PREOP {
                    kind: Parselet::PREFIX,
                    token: name_token,
                    operand: Box::new(self.parse_next_expression()),
                }
            }
            _ => panic!("illegal prefix expression"),
        };

        let infix = match self.next_token {
            // * INFIX
            Tk::PLUS(_, _) | Tk::MINUS(_, _) => {
                let name_token = self.current_token.clone();
                self.consume_token();

                Expression::OPERATOR {
                    kind: Parselet::BINARY,
                    left: Box::new(left),
                    token: name_token,
                    right: Box::new(self.parse_next_expression()),
                }
            }
            // * POSTFIX
            Tk::BANG(_, _) => Expression::POSTOP {
                kind: Parselet::POSTFIX,
                left: Box::new(left),
                token: self.next_token.clone(),
            },
            // * MIXFIX
            Tk::QUESTION(_, _) => {
                self.consume_token();
                self.consume_token();
                let then_branch = self.parse_next_expression();
                self.consume_token();
                self.consume_token();
                let otherwise_branch = self.parse_next_expression();

                Expression::BRANCH {
                    kind: Parselet::COND,
                    left: Box::new(left),
                    then: Box::new(then_branch),
                    otherwise: Box::new(otherwise_branch),
                }
            }
            // * PREFIX
            _ => return left,
        };

        self.consume_token();

        infix
    }

    fn consume_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = self.tokens.next().unwrap_or(Tk::ILEGAL);
    }

    fn lookahead_by(&self, distance: usize) {
        // * quizá esta funcionalidad será remplazada por
        // * self#next_token, self#current_token, self#next_token
        // * si obtenemos el tamaño del iterador, se va consumir potencialmente e innecesariamente ❗❓
        // * en otras implementaciones se asume que ya están en memoria los tokens
        // * @see https://github.com/Mountagha/bantam/blob/main/parser.py
        todo!()
    }

    fn parse_prefix(&self) {
        todo!()
    }

    fn parse_infix(&self) {
        todo!()
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

        let parsed = parser.parse_next_expression();
        assert_eq!(
            parsed,
            Expression::PREOP {
                kind: Parselet::PREFIX,
                token: Tk::MINUS("-".to_string(), 1),
                operand: Box::new(Expression::NAME {
                    kind: Parselet::PREFIX,
                    token: Tk::NAME("a".to_string(), 1)
                })
            }
        );
    }

    #[test]
    fn it_works_with_infixes() {
        let unary_ops = [
            ("-a + b", Tk::PLUS("+".to_string(), 1)),
            ("-a - b", Tk::MINUS("-".to_string(), 1)),
        ];

        for (input, expected_token) in unary_ops {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression();
            assert_eq!(
                parsed,
                Expression::PREOP {
                    kind: Parselet::PREFIX,
                    token: Tk::MINUS("-".to_string(), 1),
                    operand: Box::new(Expression::OPERATOR {
                        kind: Parselet::BINARY,
                        left: Box::new(Expression::NAME {
                            kind: Parselet::PREFIX,
                            token: Tk::NAME("a".to_string(), 1)
                        }),
                        token: Tk::NAME("a".to_string(), 1),
                        right: Box::new(Expression::PREOP {
                            kind: Parselet::PREFIX,
                            token: expected_token,
                            operand: Box::new(Expression::NAME {
                                kind: Parselet::PREFIX,
                                token: Tk::NAME("b".to_string(), 1)
                            })
                        })
                    })
                }
            );
        }
    }

    #[test]
    fn it_works_with_postfixes() {
        let unary_ops = [("a!", Tk::BANG("!".to_string(), 1))];

        for (input, expected_token) in unary_ops {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression();
            assert_eq!(
                parsed,
                Expression::POSTOP {
                    kind: Parselet::POSTFIX,
                    left: Box::new(Expression::NAME {
                        kind: Parselet::PREFIX,
                        token: Tk::NAME("a".to_string(), 1)
                    }),
                    token: expected_token
                }
            );
        }
    }

    #[test]
    fn it_works_with_conditional() {
        let unary_ops = [("a ? b : c", Tk::BANG("!".to_string(), 1))];

        for (input, expected_token) in unary_ops {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression();
            assert_eq!(
                parsed,
                Expression::BRANCH {
                    kind: Parselet::COND,
                    left: Box::new(Expression::NAME {
                        kind: Parselet::PREFIX,
                        token: Tk::NAME("a".to_string(), 1)
                    }),
                    then: Box::new(Expression::NAME {
                        kind: Parselet::PREFIX,
                        token: Tk::NAME("b".to_string(), 1)
                    }),
                    otherwise: Box::new(Expression::NAME {
                        kind: Parselet::PREFIX,
                        token: Tk::NAME("c".to_string(), 1)
                    })
                }
            );
        }
    }

    #[test]
    fn it_works_with_complex_conditional() {
        let unary_ops = [("a ? b! : -c", Tk::BANG("!".to_string(), 1))];

        for (input, expected_token) in unary_ops {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression();
            assert_eq!(
                parsed,
                Expression::BRANCH {
                    kind: Parselet::COND,
                    left: Box::new(Expression::NAME {
                        kind: Parselet::PREFIX,
                        token: Tk::NAME("a".to_string(), 1)
                    }),
                    then: Box::new(Expression::POSTOP {
                        kind: Parselet::POSTFIX,
                        left: Box::new(Expression::NAME {
                            kind: Parselet::PREFIX,
                            token: Tk::NAME("b".to_string(), 1)
                        }),
                        token: Tk::BANG("!".to_string(), 1)
                    }),
                    otherwise: Box::new(Expression::PREOP {
                        kind: Parselet::PREFIX,
                        token: Tk::MINUS("-".to_string(), 1),
                        operand: Box::new(Expression::NAME {
                            kind: Parselet::PREFIX,
                            token: Tk::NAME("c".to_string(), 1)
                        })
                    })
                }
            );
        }
    }
}
