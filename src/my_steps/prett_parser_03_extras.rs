use std::{collections::HashMap, usize};

use crate::pratt_lexer_01::{Lexer, Tk};

#[derive(PartialEq, Debug, Clone)]
enum Parselet {
    NAME,
    UNARY,
    POSTFIX,
    BINARY,
    BRANCH,
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

//?     public static final int ASSIGNMENT  = 1;
//?     public static final int CONDITIONAL = 2;
//?     public static final int SUM         = 3;
//?     public static final int PRODUCT     = 4;
//?     public static final int EXPONENT    = 5;
//?     public static final int PREFIX      = 6;
//?     public static final int POSTFIX     = 7;
//?     public static final int CALL        = 8;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
enum Precedence {
    DEFAULT,
    AssignOrElse,
    COND,
    SUM,
    MUL,
    POW,
    PREFIX,
    POST,
    CALL,
}

impl From<Tk> for Precedence {
    fn from(value: Tk) -> Precedence {
        match value {
            // Tk::LPAREN(_, _) => todo!(),
            // Tk::RPAREN(_, _) => todo!(),
            // Tk::COMMA(_, _) => todo!(),
            // Tk::ASSIGN(_, _) => todo!(),
            // Tk::ASTERISK(_, _) => todo!(),
            // Tk::SLASH(_, _) => todo!(),
            // Tk::CARET(_, _) => todo!(),
            // Tk::TILDE(_, _) | Tk::PLUS(_, _) | Tk::MINUS(_, _) => Precedence::PRE,
            Tk::PLUS(_, _) | Tk::MINUS(_, _) => Precedence::SUM,
            // * aquí puede haber conflicto, por que bang puede ser prefijo e infijo
            // Tk::BANG(_, _) => Precedence::POST,
            // Tk::QUESTION(_, _) => Precedence::COND,
            // Tk::COLON(_, _) => todo!(),
            // Tk::NAME(_, _) => Precedence::DEFAULT,
            // Tk::EOF(_, _) => todo!(),
            // Tk::ILEGAL(_, _) => todo!(),
            _ => Precedence::DEFAULT,
        }
    }
}

trait Pratt {
    fn parse_next_expression(&mut self, precedence: Precedence) -> Expression;
    // fn parse_next_infix_expression(
    //     &mut self,
    //     left: Expression,
    //     precedence: Precedence,
    // ) -> Expression;
    fn consume_token(&mut self);
    fn lookahead_by(&self, distance: usize);
    fn get_precedence(&self) -> u8;
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
            token: self.current_token.clone(),
            kind: Parselet::UNARY,
        }
    }

    fn parse_infix_exp(&mut self, left: &Expression) -> Expression {
        let name_token = self.current_token.clone();

        let current_precedence = Precedence::from(self.current_token.clone());
        self.consume_token();

        Expression::OPERATOR {
            kind: Parselet::BINARY,
            left: Box::new(left.clone()),
            operator: name_token,
            right: Box::new(
                // self.parse_next_infix_expression(left.clone(), Precedence::SUM),
                self.parse_next_expression(current_precedence),
            ),
        }
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        // * null denotation aka nuds
        match self.current_token {
            Tk::NAME(_, _) => Some(self.parse_id_exp()),
            // Tk::PLUS(_, _) => self.parse_prefix_exp(),
            Tk::MINUS(_, _) => Some(self.parse_prefix_exp()),
            // Tk::TILDE(_, _) => self.parse_prefix_exp(),
            Tk::BANG(_, _) => Some(self.parse_prefix_exp()),
            // _ => panic!("illegal prefix expression"),
            _ => None,
        }
    }

    fn parse_infixes(&mut self, left_expression: &Expression) -> Option<bool> {
        // * left denotations aka nuds
        match self.next_token {
            // * INFIX
            Tk::PLUS(_, _) => Some(true),
            Tk::MINUS(_, _) => Some(true),
            // // * POSTFIX
            // Tk::BANG(_, _) => Expression::POSTOP {
            //     kind: Parselet::POSTFIX,
            //     left: Box::new(left),
            //     token: self.next_token.clone(),
            // },
            // // * MIXFIX
            Tk::QUESTION(_, _) => {
                self.consume_token();
                self.consume_token();
                let then_branch =
                        // self.parse_next_infix_expression(left.clone(), Precedence::DEFAULT);
                        self.parse_next_expression(Precedence::DEFAULT);
                self.consume_token();
                self.consume_token();
                let otherwise_branch =
                        // self.parse_next_infix_expression(left.clone(), Precedence::AssignOrElse);
                        self.parse_next_expression(Precedence::AssignOrElse);

                Expression::BRANCH {
                    kind: Parselet::BRANCH,
                    left: Box::new(left),
                    then: Box::new(then_branch),
                    otherwise: Box::new(otherwise_branch),
                }
            }
            // * PREFIX
            // _ => left_expression.clone(),
            _ => None,
        }
    }
}

impl Pratt for Parser {
    // fn parse_next_infix_expression(
    //     &mut self,
    //     left: Expression,
    //     precedence: Precedence,
    // ) -> Expression {
    //     println!(
    //         "parsing infix - {:?} - pre {:?}",
    //         self.current_token, precedence
    //     );

    //     match self.current_token {
    //         // * INFIX
    //         Tk::PLUS(_, _) | Tk::MINUS(_, _) => {
    //             let name_token = self.current_token.clone();
    //             self.consume_token();

    //             Expression::OPERATOR {
    //                 kind: Parselet::BINARY,
    //                 left: Box::new(left),
    //                 token: name_token,
    //                 right: Box::new(self.parse_next_expression(Precedence::SUM)),
    //             }
    //         }
    //         // * POSTFIX
    //         Tk::BANG(_, _) => Expression::POSTOP {
    //             kind: Parselet::POSTFIX,
    //             left: Box::new(left),
    //             token: self.next_token.clone(),
    //         },
    //         // * MIXFIX
    //         Tk::QUESTION(_, _) => {
    //             self.consume_token();
    //             self.consume_token();
    //             let then_branch = self.parse_next_expression(Precedence::DEFAULT);
    //             self.consume_token();
    //             self.consume_token();
    //             let otherwise_branch = self.parse_next_expression(Precedence::AssignOrElse);

    //             Expression::BRANCH {
    //                 kind: Parselet::BRANCH,
    //                 left: Box::new(left),
    //                 then: Box::new(then_branch),
    //                 otherwise: Box::new(otherwise_branch),
    //             }
    //         }
    //         // * PREFIX
    //         _ => panic!("invalid infix expression!"),
    //     }
    // }

    fn parse_next_expression(&mut self, precedence: Precedence) -> Expression {
        // self.consume_token();
        println!("parsing - {:?} - pre {:?}", self.current_token, precedence);

        // * null denotation aka nuds
        let mut left_expr = match self.current_token {
            Tk::NAME(_, _) => self.parse_id_exp(),
            // Tk::PLUS(_, _) => self.parse_prefix_exp(),
            Tk::MINUS(_, _) => self.parse_prefix_exp(),
            // Tk::TILDE(_, _) => self.parse_prefix_exp(),
            Tk::BANG(_, _) => self.parse_prefix_exp(),
            _ => panic!("illegal prefix expression {:?}", self.current_token),
            // _ => None,
        };

        // println!(
        //     "pre - {:?} - nex {:?} entra? {:?}, tok - {:?}",
        //     precedence.clone() as i32,
        //     Precedence::from(self.next_token.clone()) as i32,
        //     precedence.clone() < Precedence::from(self.next_token.clone()),
        //     self.next_token.clone(),
        // );

        while precedence < Precedence::from(self.next_token.clone()) {
            // * left denotations aka nuds 😏
            match self.next_token {
                // * INFIX
                Tk::PLUS(_, _) => true,
                Tk::MINUS(_, _) => true,
                _ => return left_expr,
            };

            self.consume_token();

            left_expr = match self.current_token {
                // * INFIX
                Tk::PLUS(_, _) => self.parse_infix_exp(&left_expr),
                Tk::MINUS(_, _) => self.parse_infix_exp(&left_expr),
                _ => panic!("no debe llegar aquí!❌"),
            }
        }

        left_expr
    }

    fn consume_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = self.tokens.next().unwrap_or(Tk::ILEGAL);
        println!("consumed {:?}", self.current_token);
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

    fn get_precedence(&self) -> u8 {
        0
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
                    kind: Parselet::UNARY,
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
            let parsed = parser.parse_next_expression(Precedence::DEFAULT);
            assert_eq!(
                parsed,
                Expression::PREOP {
                    kind: Parselet::UNARY,
                    operator: Tk::MINUS("-".to_string(), 1),
                    operand: Box::new(Expression::OPERATOR {
                        kind: Parselet::BINARY,
                        left: Box::new(Expression::NAME {
                            kind: Parselet::UNARY,
                            token: Tk::NAME("a".to_string(), 1)
                        }),
                        operator: Tk::NAME("a".to_string(), 1),
                        right: Box::new(Expression::PREOP {
                            kind: Parselet::UNARY,
                            operator: expected_token,
                            operand: Box::new(Expression::NAME {
                                kind: Parselet::UNARY,
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
            let parsed = parser.parse_next_expression(Precedence::DEFAULT);
            assert_eq!(
                parsed,
                Expression::POSTOP {
                    kind: Parselet::POSTFIX,
                    left: Box::new(Expression::NAME {
                        kind: Parselet::UNARY,
                        token: Tk::NAME("a".to_string(), 1)
                    }),
                    token: expected_token
                }
            );
        }
    }

    #[test]
    fn it_works_with_conditional() {
        let unary_ops = [("a ? b : c", Tk::ILEGAL)];

        for (input, expected_token) in unary_ops {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression(Precedence::DEFAULT);
            assert_eq!(
                parsed,
                Expression::BRANCH {
                    kind: Parselet::BRANCH,
                    left: Box::new(Expression::NAME {
                        kind: Parselet::UNARY,
                        token: Tk::NAME("a".to_string(), 1)
                    }),
                    then: Box::new(Expression::NAME {
                        kind: Parselet::UNARY,
                        token: Tk::NAME("b".to_string(), 1)
                    }),
                    otherwise: Box::new(Expression::NAME {
                        kind: Parselet::UNARY,
                        token: Tk::NAME("c".to_string(), 1)
                    })
                }
            );
        }
    }

    #[test]
    fn it_works_with_complex_conditional() {
        let unary_ops = [("a ? b! : -c", Tk::ILEGAL)];

        for (input, expected_token) in unary_ops {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression(Precedence::DEFAULT);
            assert_eq!(
                parsed,
                Expression::BRANCH {
                    kind: Parselet::BRANCH,
                    left: Box::new(Expression::NAME {
                        kind: Parselet::UNARY,
                        token: Tk::NAME("a".to_string(), 1)
                    }),
                    then: Box::new(Expression::POSTOP {
                        kind: Parselet::POSTFIX,
                        left: Box::new(Expression::NAME {
                            kind: Parselet::UNARY,
                            token: Tk::NAME("b".to_string(), 1)
                        }),
                        token: Tk::BANG("!".to_string(), 1)
                    }),
                    otherwise: Box::new(Expression::PREOP {
                        kind: Parselet::UNARY,
                        operator: Tk::MINUS("-".to_string(), 1),
                        operand: Box::new(Expression::NAME {
                            kind: Parselet::UNARY,
                            token: Tk::NAME("c".to_string(), 1)
                        })
                    })
                }
            );
        }
    }

    #[test]
    fn it_works_with_associative_left() {
        // let unary_ops = [("a - b - c", Tk::ILEGAL)];
        // let unary_ops = [("a - b", Tk::ILEGAL)];
        let unary_ops = [("a + b", Tk::ILEGAL)];

        for (input, expected_token) in unary_ops {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression(Precedence::DEFAULT);
            assert_eq!(
                parsed,
                // * (a - b) - c
                Expression::OPERATOR {
                    kind: Parselet::BINARY,
                    left: Box::new(Expression::NAME {
                        kind: Parselet::UNARY,
                        token: Tk::NAME("a".to_string(), 1)
                    }),
                    operator: Tk::PLUS("+".to_string(), 1),
                    right: Box::new(Expression::NAME {
                        kind: Parselet::UNARY,
                        token: Tk::NAME("b".to_string(), 1)
                    })
                },
                // Expression::NONE // Expression::OPERATOR {
                //     kind: Parselet::BINARY,
                //     left: Box::new(Expression::NAME {
                //         kind: Parselet::PREFIX,
                //         token: Tk::NAME("a".to_string(), 1)
                //     }),
                //     token: Tk::NAME("a".to_string(), 1),
                //     right: Box::new(Expression::PREOP {
                //         kind: Parselet::PREFIX,
                //         token: Tk::MINUS("-".to_string(), 1),
                //         operand: Box::new(Expression::NAME {
                //             kind: Parselet::PREFIX,
                //             token: Tk::NAME("b".to_string(), 1)
                //         })
                //     })
                // },
                // * a - (b - c)
                // * right associative by default
                // Expression::NONE
                // Expression::OPERATOR {
                //     kind: Parselet::BINARY,
                //     left: Box::new(Expression::NAME {
                //         kind: Parselet::PREFIX,
                //         token: Tk::NAME("a".to_string(), 1)
                //     }),
                //     token: Tk::NAME("a".to_string(), 1),
                //     right: Box::new(Expression::PREOP {
                //         kind: Parselet::PREFIX,
                //         token: Tk::MINUS("-".to_string(), 1),
                //         operand: Box::new(Expression::OPERATOR {
                //             kind: Parselet::BINARY,
                //             left: Box::new(Expression::NAME {
                //                 kind: Parselet::PREFIX,
                //                 token: Tk::NAME("b".to_string(), 1)
                //             }),
                //             token: Tk::NAME("b".to_string(), 1),
                //             right: Box::new(Expression::PREOP {
                //                 kind: Parselet::PREFIX,
                //                 token: Tk::MINUS("-".to_string(), 1),
                //                 operand: Box::new(Expression::NAME {
                //                     kind: Parselet::PREFIX,
                //                     token: Tk::NAME("c".to_string(), 1)
                //                 })
                //             })
                //         })
                //     })
                // }
            );
        }
    }
}
