use crate::pratt_lexer_01::{Lexer, Tk};
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
enum Parselet {
    ID,
    UNARY,
    BINARY,
    BRANCH,
}

#[derive(PartialEq, Debug, Clone)]
enum Expression {
    NONE,
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
        operand: Box<Expression>,
        operator: Tk,
    },
    BRANCH {
        kind: Parselet,
        left: Box<Expression>,
        then: Box<Expression>,
        otherwise: Box<Expression>,
    },
    COMPARISON {
        kind: Parselet,
        operands: Vec<Box<Expression>>,
    },
    BOOLEAN {
        kind: Parselet,
        value: Tk,
    },
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
            Expression::POSTOP {
                kind,
                operand,
                operator,
            } => write!(
                f,
                "({}{})",
                operand.to_string(),
                String::from(operator.clone()),
            )?,
            Expression::BRANCH {
                kind,
                left,
                then,
                otherwise,
            } => write!(
                f,
                "({} ? {} : {})",
                left.to_string(),
                then.to_string(),
                otherwise.to_string(),
            )?,
            Expression::COMPARISON { kind, operands } => {
                //? a < b
                //? a < b < c
                //? a < b < c < d
                write!(f, "(")?;
                for (i, op) in operands.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{}", op.to_string(),)?;
                    } else {
                        write!(f, " < {}", op.to_string(),)?;
                    }
                }
                write!(f, ")")?;
            }
            Expression::NONE => write!(f, "__test__")?,
            Expression::BOOLEAN { kind, value } => match value {
                Tk::TRUE(_, _) => write!(f, "#t")?,
                Tk::FALSE(_, _) => write!(f, "#f")?,
                _ => panic!("❌ no deberia estar aquí"),
            },
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
    BRANCH,
    COMPARE,
    SUM,
    PRODUCT,
    PREFIX,
    POSTFIX,
    //? ASSIGNMENT  = 1;
    //? CONDITIONAL = 2;
    //? SUM         = 3;
    //? PRODUCT     = 4;
    //? EXPONENT    = 5;
    //? PREFIX      = 6;
    //? POSTFIX     = 7;
    //? CALL        = 8;
}

impl From<Tk> for Precedence {
    fn from(value: Tk) -> Precedence {
        match value {
            Tk::PLUS(_, _) => Precedence::SUM,
            Tk::MINUS(_, _) => Precedence::SUM,
            Tk::ASTERISK(_, _) => Precedence::PRODUCT,
            Tk::SLASH(_, _) => Precedence::PRODUCT,
            Tk::BANG(_, _) => Precedence::POSTFIX,
            Tk::QUESTION(_, _) => Precedence::BRANCH,
            Tk::LTHAN(_, _) => Precedence::COMPARE,
            Tk::EOF(_, _) => Precedence::DEFAULT,
            Tk::LPAREN(_, _) => Precedence::DEFAULT,
            Tk::RPAREN(_, _) => Precedence::DEFAULT,
            Tk::COMMA(_, _) => Precedence::DEFAULT,
            Tk::ASSIGN(_, _) => Precedence::DEFAULT,
            Tk::CARET(_, _) => Precedence::DEFAULT,
            Tk::TILDE(_, _) => Precedence::DEFAULT,
            Tk::COLON(_, _) => Precedence::DEFAULT,
            Tk::NAME(_, _) => Precedence::DEFAULT,
            Tk::ILEGAL => Precedence::DEFAULT,
            Tk::TRUE(_, _) => Precedence::DEFAULT,
            Tk::FALSE(_, _) => Precedence::DEFAULT,
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

    fn parse_prefix(&mut self) -> Expression {
        let name_token = self.current_token.clone();
        self.consume_token();

        // * unary expression at least have one operand
        Expression::PREOP {
            kind: Parselet::UNARY,
            operator: name_token,
            operand: Box::new(self.parse_next_expression(Precedence::PREFIX)),
        }
    }

    fn parse_identifier(&mut self) -> Expression {
        //? id parselet should be considered unary❓
        Expression::NAME {
            kind: Parselet::ID,
            token: self.current_token.clone(),
        }
    }

    fn parse_infix(&mut self, left: &Expression) -> Expression {
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
    // * The algorithm behind the parseExpression method and its combination of parsing functions
    // * and precedences is fully described by Vaughan Pratt in his “Top Down Operator Precedence”
    // * paper.
    /*
     * A + B + C;
     * The big challenge here is not to represent every operator and operand
     * in the resulting AST, but to nest the nodes of the AST correctly.
     * What we want is an AST Program Node that (serialized as a string) looks like this:
     * ((A + B) + C), esto es que sea asociativo hacia la izquierda 💃.
     */
    fn parse_next_expression(&mut self, precedence: Precedence) -> Expression {
        /*
         *✨ FIRST CALL ❗
         * Here is what happens when we parse A + B + C;:
         * calls 🐋parse_next_expression(DEFAULT).
         *
         * #current_token   -> A
         * #next_token  	-> ➕
         */
        /*
         * ✨ SECOND CALL ❗
         * The first thing 🐋parse_next_expression does again is to look for a
         *  prefixParseFn for p.curToken. And again it’s parseIntegerLiteral
         *
         * #current_token   -> B
         * #next_token  	-> ➕
         */
        println!("parsing - {:?} - pre {:?}", self.current_token, precedence);

        /*
         * The first thing #parse_next_expression then does is to check whether
         * there is a PARSE FUNCTION associated with the current TOKEN,
         *  which is a TK#NAME. And, yes, there is: #parse_identifier.
         *  So it calls #parse_identifier, which returns an EXPRESSION.
         *  match assigns this to left_expr.
         */
        //? null denotations aka nuds😏
        let mut left_expr = match self.current_token {
            Tk::NAME(_, _) => self.parse_identifier(),
            Tk::MINUS(_, _) => self.parse_prefix(),
            Tk::BANG(_, _) => self.parse_prefix(),
            Tk::TRUE(_, _) => Expression::BOOLEAN {
                kind: Parselet::UNARY,
                value: self.current_token.clone(),
            },
            Tk::FALSE(_, _) => Expression::BOOLEAN {
                kind: Parselet::UNARY,
                value: self.current_token.clone(),
            },
            //? grouping
            Tk::LPAREN(_, _) => {
                self.consume_token();

                let expr = self.parse_next_expression(Precedence::DEFAULT);

                match self.next_token {
                    Tk::RPAREN(_, _) => {
                        self.consume_token();
                        expr
                    }
                    _ => panic!("\n🎈wtf is this {:?} 💩!", self.next_token),
                }
            }
            _ => panic!("\n❌ illegal prefix expression {:?}", self.current_token),
        };

        // while self.next_token != Tk::EOF(_, _) && precedence < Precedence::from(self.next_token.clone()) {
        /*
         * 👀 This condition checks if the left-binding power of the next operator/token
         * is higher than our current right-binding power.
         * If it is, what we parsed so far gets “sucked in” by the next operator,
         *  from left to right, and ends up being passed to the
         * infix parse function of the next operator.
         */
        /*
         * Precedence::from(self.next_token.clone()) (which returns the precedence of the + token)
         * is higher than the argument passed to parseExpression, which is DEFAULT.
         * Here are our defined precedences again to refresh our memory:
         */
        //* ASSIGNMENT  = 1;
        //* BRACH       = 2;
        //* SUM         = 3;
        //* PRODUCT     = 4;
        //* EXPONENT    = 5;
        //* PREFIX      = 6;
        //* POSTFIX     = 7;
        //* CALL        = 8;

        //* ✨ FIRST CALL ❗
        //* So the condition evaluates to true and 🐋parse_next_expression
        //* executes the body of the loop.
        /*
         * ✨ SECOND CALL ❗
         * But now the condition of the while doesn’t evaluate to true:
         * precedence (the argument passed to 🐋parse_next_expression) is the precedence
         * of the first ➕ operator in 1 + 2 + 3, which is not smaller than the
         * precedence of #next_token, the second ➕ operator. They are equal.
         *
         * then EXPRESSION representing the B in left_expr is returned
         */
        //? while RIGHT_BINDING_POWER < LEFT_BINDING_POWER
        //? THEN suck it to the LEFT SIDE
        while precedence < Precedence::from(self.next_token.clone()) {
            /*
             * ✨ FIRST LOOP ❗
             * Now it verifies if there is a PARSING FUNCTION for #next_token, which is
             * TRUE defined on the match below.
             * ⁉❓ @phau: aquí lo cambie por que muchos implementaban un hash_map
             * donde mapeaban un token a una función, en rust seguro podría hacer
             * un mapeo tipo token a un function pointer pero me pareció algo elaborado
             * en este momento, aunque queda un poco repetido la lógica del match, no
             * lo veo mal por ahora 👀.
             */
            /*
             * ✨ SECOND LOOP ❗
             *  we’re back in the outer-most call to 🐋parse_next_expression,
             *  where precedence is still DEFAULT from the initial call
             *
             * This still evaluates to true, since precedence is DEFAULT and
             * #next_token now returns the precedence of the
             * second ➕ in our expression, which is higher
             *
             * The difference is that now left_Expr is not an EXPRESSION
             * representing the A, but the INFIX EXPRESSION
             * returned representing (A + B).
             */
            /*
             * ✨ THIRD LOOP ❗
             * The condition of the while evaluates to false
             *
             * Precedence::from(self.next_token.clone())
             * method returns DEFAULT as the value
             */
            //? left denotations aka leds
            match self.next_token {
                Tk::PLUS(_, _) => true,
                Tk::MINUS(_, _) => true,
                Tk::ASTERISK(_, _) => true,
                Tk::SLASH(_, _) => true,
                Tk::BANG(_, _) => true,
                Tk::QUESTION(_, _) => true,
                Tk::LTHAN(_, _) => true,
                _ => return left_expr,
            };

            self.consume_token();
            /*
             * ✨ FIRST LOOP ❗
             * Before calling it and assigning its return value to
             * left_expr (reusing the left_expr variable!)
             * it advances the tokens so they now look like this:
             *
             * A ➕ B ➕ C;
             * #current_token   -> ➕
             * #next_token  	-> B
             */

            /*
             * ✨ SECOND LOOP ❗
             * A ➕ B ➕ C;
             * #current_token   -> ➕
             * #next_token  	-> C
             */

            left_expr = match self.current_token {
                //? infixes
                Tk::PLUS(_, _) => {
                    let name_token = self.current_token.clone();
                    //* saves the precedence of 🐋current_token (the first ➕ token!),
                    let current_precedence = Precedence::from(self.current_token.clone());
                    //* advances the tokens and calls 🐋parse_next_expression
                    //* - passing in the just saved precedence.
                    self.consume_token();
                    /*
                     * ✨ FIRST LOOP ❗
                     * A ➕ B ➕ C;
                     * #current_token   -> B
                     * #next_token  	-> ➕
                     */
                    /*
                     * ✨ SECOND LOOP ❗
                     * A ➕ B ➕ C;
                     * #current_token   -> C
                     * #next_token  	-> EOF
                     */
                    Expression::OPERATOR {
                        kind: Parselet::BINARY,
                        left: Box::new(left_expr),
                        operator: name_token,
                        //* ✨ FIRST LOOP ❗
                        //* So now 🐋parse_next_expression is called the second time❗
                        //* the return-value of 🐋parse_next_expression the B expression
                        //* is assigned to the Right field of the newly constructed Expression.
                        /*
                         * ✨ SECOND LOOP ❗
                         * the return-value of 🐋parse_next_expression the C expression
                         */
                        right: Box::new(self.parse_next_expression(current_precedence)),
                    }
                }
                Tk::MINUS(_, _) => self.parse_infix(&left_expr),
                Tk::ASTERISK(_, _) => self.parse_infix(&left_expr),
                Tk::SLASH(_, _) => self.parse_infix(&left_expr),
                //? postfixes
                Tk::BANG(_, _) => Expression::POSTOP {
                    kind: Parselet::UNARY,
                    operand: Box::new(left_expr),
                    operator: self.current_token.clone(),
                },
                //? branch
                Tk::QUESTION(_, _) => {
                    self.consume_token();
                    let then_branch = self.parse_next_expression(Precedence::DEFAULT);

                    self.consume_token();

                    self.consume_token();
                    let otherwise_branch = self.parse_next_expression(Precedence::DEFAULT);

                    Expression::BRANCH {
                        kind: Parselet::BRANCH,
                        left: Box::new(left_expr),
                        then: Box::new(then_branch),
                        otherwise: Box::new(otherwise_branch),
                    }
                }
                //? comparison
                Tk::LTHAN(_, _) => {
                    let mut operands = vec![];
                    operands.push(Box::new(left_expr));
                    self.consume_token();
                    operands.push(Box::new(self.parse_next_expression(Precedence::DEFAULT)));
                    // todo: improve this while 💩❗
                    while let Some(_value) = match self.next_token {
                        Tk::LTHAN(_, _) => Some(()),
                        _ => None,
                    } {
                        self.consume_token();
                        operands.push(Box::new(self.parse_next_expression(Precedence::DEFAULT)));
                    }

                    Expression::COMPARISON {
                        kind: Parselet::BRANCH,
                        operands,
                    }
                }
                _ => panic!("no debe llegar aquí!❌"),
            }
        }
        // * After all this, at the end of the loop-body, left_Exp looks like this
        // * The goal is to have expressions involving operators with a
        // * higher precedence to be deeper in the tree than expressions
        // * with lower precedence operators. This is accomplished by the
        // * precedence value (the argument)
        // * ((A + B) + C)
        left_expr
    }

    fn consume_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = self.tokens.next().unwrap_or(Tk::ILEGAL);
        println!("consumed {:?}", self.current_token);
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
    fn it_works_with_debug_strings() {
        let tests = [
            ("-a + b", "((-a) + b)"),
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            //? precedence
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            //? postfix
            ("a!", "(a!)"),
            ("-a!", "(-(a!))"),
            //? conditionals
            ("a ? b : c", "(a ? b : c)"),
            ("a ? b! : -c", "(a ? (b!) : (-c))"),
            ("a + b ? c! : -d", "((a + b) ? (c!) : (-d))"),
            //? comparisons
            ("a < b", "(a < b)"),
            ("a + b < c + d", "((a + b) < (c + d))"),
            ("a < b < c", "(a < (b < c))"),
            //? booleans
            ("true", "#t"),
            ("false", "#f"),
            ("a < b < false", "(a < (b < #f))"),
            ("!true", "(!#t)"),
            ("!false", "(!#f)"),
            //? grouping 😃
            ("a + (b + c) + d", "((a + (b + c)) + d)"),
            ("(a + a) * b", "((a + a) * b)"),
            ("a / (b + b)", "(a / (b + b))"),
            ("-(a + b)", "(-(a + b))"),
            //* aun no tenemos assignments como expresión
            // ("!(a = a)", "(!(a = a))"),
            ("!(a + a)", "(!(a + a))"),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse_next_expression(Precedence::DEFAULT);

            assert_eq!(parsed.to_string(), expected.to_string());
        }
    }
}
