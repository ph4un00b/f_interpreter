use std::fmt::{self, Display};

fn string_err(expected_token: Tk, token: Tk, context_message: &str) -> String {
    if token == Tk::End {
        format!(
            "❌ expected next token to be {:?} at end, {}, got {:?} instead",
            expected_token, context_message, token
        )
    } else if expected_token == Tk::Default {
        format!("❌ {}, got {:?} instead", context_message, token)
    } else {
        format!(
            "❌ expected next token to be {:?}, {}, got {:?} instead",
            expected_token, context_message, token
        )
    }
}

fn lox_report_error(expected_token: Tk, token: Tk, context_message: &str) {
    /*
     * Another way to handle common syntax errors is with error productions.
     * You augment the grammar with a rule that
     * successfully matches the erroneous syntax. The parser
     * safely parses it but then reports it as an error instead
     * of producing a syntax tree.
     *
     * unary → ( "!" | "-" | -> "+" <- ) unary | primary ;
     *
     * This lets the parser consume + without going into
     * panic mode or leaving the parser in a weird state
     *
     * This lets the parser consume + without going into panic
     * mode or leaving the parser in a weird state
     */

    //? Mature parsers tend to accumulate error productions
    //? like barnacles since they help users fix common mistakes.
    if token == Tk::End {
        eprintln!(
            "❌ expected next token to be {:?} at end, {}, got {:?} instead",
            expected_token, context_message, token
        );
    } else {
        eprintln!(
            "❌ expected next token to be {:?}, {}, got {:?} instead",
            expected_token, context_message, token
        );
    }
}
/*
 * minimal parser and token for
 * 1 - (2 * 3) < 4 == false
 */
enum Expr {
    Unary {
        operator: Tk,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Tk,
        right: Box<Expr>,
    },
    Literal(Value),
    Grouping(Box<Expr>),
    None,
}

enum Value {
    I32(i32),
    F64(f64),
    String(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I32(val) => write!(f, "{}", val)?,
            Value::F64(val) => write!(f, "{}", val)?,
            Value::String(val) => write!(f, "{}", val)?,
        }
        Ok(())
    }
}

impl From<Tk> for String {
    fn from(value: Tk) -> String {
        match value {
            Tk::Num(val) => val.to_string(),
            Tk::Float(val) => val.to_string(),
            Tk::Sub => "-".to_string(),
            Tk::Lpar => "(".to_string(),
            Tk::Mul => "*".to_string(),
            Tk::Rpar => ")".to_string(),
            Tk::LT => "<".to_string(),
            Tk::EQ => "==".to_string(),
            Tk::False => "false".to_string(),
            Tk::Default => unreachable!(),
            Tk::End => unreachable!(),
            Tk::Bang => "!".to_string(),
            Tk::Semi => ";".to_string(),
            Tk::Class => "class".to_string(),
            Tk::Fn => "fn".to_string(),
            Tk::Var => "let".to_string(),
            Tk::For => "for".to_string(),
            Tk::If => "if".to_string(),
            Tk::While => "while".to_string(),
            Tk::Print => "put".to_string(),
            Tk::Return => "ret".to_string(),
        }
    }
}

//? 1 - (2 * 3) < 4 == false
#[derive(Clone, PartialEq, Debug)]
#[allow(dead_code)]
enum Tk {
    Default,
    Num(i32),
    Float(f64),
    Sub,
    Lpar,
    Mul,
    Rpar,
    LT,
    EQ,
    False,
    Bang,
    End,
    Semi,
    Class,
    Fn,
    Var,
    For,
    If,
    While,
    Print,
    Return,
}

trait ToLiteral<TValue>
where
    TValue: Display,
{
    fn literal(val: TValue) -> Expr;
}

impl ToLiteral<i32> for Expr {
    fn literal(val: i32) -> Self {
        Expr::Literal(Value::I32(val))
    }
}

impl ToLiteral<String> for Expr {
    fn literal(val: String) -> Self {
        Expr::Literal(Value::String(val))
    }
}

impl ToLiteral<&str> for Expr {
    fn literal(val: &str) -> Self {
        Expr::Literal(Value::String(val.to_string()))
    }
}

impl ToLiteral<f64> for Expr {
    fn literal(val: f64) -> Self {
        Expr::Literal(Value::F64(val))
    }
}

impl Expr {
    fn binary_op(left: Expr, operator: Tk, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn unary_op(operator: Tk, right: Expr) -> Self {
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    fn grouping(expression: Expr) -> Self {
        Expr::Grouping(Box::new(expression))
    }

    // Other methods...
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unary { operator, right } => {
                let token = operator.clone();
                write!(f, "({}{})", String::from(token), right)?
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", String::from(operator.clone()), left, right)?,
            Expr::Literal(val) => write!(f, "{}", val)?,
            Expr::Grouping(expr) => write!(f, "(group {})", expr)?,
            Expr::None => write!(f, "❌ algún error!")?,
        }

        Ok(())
    }
}

#[allow(dead_code)]
struct Parser {
    tokens: Vec<Tk>,
    current_position: usize,
    current_token: Tk,
    prev_token: Tk,
}

enum ParserAy {
    BadExpression(Tk),
}

use std::result::Result as StdResult;
type Result<T> = StdResult<T, ParserAy>;

#[allow(dead_code)]
impl Parser {
    fn new(tokens: Vec<Tk>) -> Self {
        let mut p = Parser {
            tokens,
            current_position: 0,
            current_token: Tk::Default,
            prev_token: Tk::Default,
        };
        p.init_tokens();
        p
    }

    /*
     * Each method for parsing a grammar rule
     * produces a syntax tree for that rule and
     * returns it to the caller. When the body of
     * the rule contains a non-terminal—a reference
     * to another rule—we call that other rule’s method.
     */
    //? This is why left recursion is problematic for
    //? recursive descent. The function for a
    //? left-recursive rule immediately calls itself,
    //? which calls itself again, and so on, until
    //? the parser hits a stack overflow and dies.
    // * equality  → comparison ( ( "!=" | "==" ) comparison )* ;
    fn init_tokens(&mut self) {
        self.current_token = self.tokens[self.current_position].clone();
        self.prev_token = Tk::Default;
    }

    fn consume_token(&mut self) {
        self.current_position += 1;
        self.current_token = self.tokens[self.current_position].clone();
        self.prev_token = self.tokens[self.current_position - 1].clone();
    }

    fn parse(&mut self) -> Expr {
        /*
         * When a syntax error does occur, this method returns null.
         * That’s OK. The parser promises not to crash or hang on
         * invalid syntax, but it doesn’t promise to return a
         * usable syntax tree if an error is found. As soon as
         * the parser reports an error, hadError gets set, and
         * subsequent phases are skipped.
         */
        self.expression().unwrap_or_else(|err| {
            match err {
                ParserAy::BadExpression(bad_token) => {
                    println!(
                        "❌ desde parse!, expected expression got {:?}😱❗",
                        bad_token
                    )
                }
            };

            Expr::None
        })
    }

    /*
     * different ways to error handling in rust 😏
     * the methods below will have the checked ✅ strategy:
     *
     * ✅ matching
     * ✅ you can use String as Errors	  Result<(), String>
     *                                      @see Parse#equality
     *                                      @see Parse#comparison
     *                                      @see Parse#term
     *                                      @see Parse#factor
     *                                      @see Parse#unary
     *                                      @see Parse#primary
     *
     * errors #1	                      let-else
     *                                      @link https://rust-lang.github.io/rfcs/3137-let-else.html
     *                                      @see Parse#parse
     *
     * ✅ errors #2	                      unwrap_or_else
     * shortcut error	                    using expect("custom error")
     * ✅ question mark operator	          maybe_Error()?.chained_maybe_error()?
     * errors #3	                        map_error
     * ✅ Enums as Errors                  Result<(), MyError>
     * yeet     	                        yeet + yeet crate
     * thiserror                            crate
     * anyhow	                            crate + trait objects
     * eyre                                 crate
     */
    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;
        while self.current_token == Tk::EQ {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.comparison()?;
            expr = Expr::binary_op(expr, operator, right);
        }
        Ok(expr)
    }

    // * comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;
        while self.current_token == Tk::LT {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.term()?;
            expr = Expr::binary_op(expr, operator, right);
        }
        Ok(expr)
    }

    // * term       = _{ factor ~ ((sub | add) ~ factor)* }
    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;
        while self.current_token == Tk::Sub || self.current_token == Tk::Mul {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.factor()?;
            expr = Expr::binary_op(expr, operator, right);
        }
        Ok(expr)
    }

    // * factor     = _{ unary ~ ((div | mul) ~ unary)* }
    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;
        while self.current_token == Tk::Mul {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.unary()?;
            expr = Expr::binary_op(expr, operator, right);
        }
        Ok(expr)
    }

    // * unary      = _{ (bang | neg) ~ unary | primary }
    fn unary(&mut self) -> Result<Expr> {
        if self.current_token == Tk::Bang || self.current_token == Tk::Sub {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.unary()?;
            return Ok(Expr::unary_op(operator, right));
        }
        self.primary()
    }

    // todo: Add support for comma expressions
    // todo: add support for the C-style conditional or “ternary” operator ?:.
    // What precedence level is allowed between the ? and :?
    // Is the whole operator left-associative or right-associative?
    // todo: Add error productions to handle each binary operator appearing without a left-hand operand.
    // In other words, detect a binary operator appearing at the beginning of an expression
    fn primary(&mut self) -> Result<Expr> {
        match self.current_token {
            Tk::False => {
                self.consume_token();
                Ok(Expr::literal("false"))
            }
            Tk::Num(val) => {
                self.consume_token();
                Ok(Expr::literal(val))
            }
            Tk::Float(val) => {
                self.consume_token();
                Ok(Expr::literal(val))
            }
            Tk::Lpar => {
                self.consume_token();
                let expr = self.expression()?;
                if self.current_token != Tk::Rpar {
                    lox_report_error(Tk::Rpar, self.current_token.clone(), "')' after EXPRESSION");
                }
                self.consume_token();
                Ok(Expr::grouping(expr))
            }
            _ => Err(ParserAy::BadExpression(self.current_token.clone())),
        }
    }

    // private void synchronize() {
    //     advance();

    //     while (!isAtEnd()) {
    //       if (previous().type == SEMICOLON) return;

    //       switch (peek().type) {
    //         case CLASS:
    //         case FUN:
    //         case VAR:
    //         case FOR:
    //         case IF:
    //         case WHILE:
    //         case PRINT:
    //         case RETURN:
    //           return;
    //       }

    //       advance();
    //     }
    //   }

    fn synchronize(&mut self) {
        self.consume_token();

        while self.current_token != Tk::End {
            if self.prev_token == Tk::Semi {
                return;
            }

            match self.current_token {
                Tk::Class
                | Tk::Fn
                | Tk::Var
                | Tk::For
                | Tk::If
                | Tk::While
                | Tk::Print
                | Tk::Return => return,
                _ => self.consume_token(),
            }
        }
    }
}

#[allow(unused)]
struct Lox {
    had_error: bool,
    had_runtime_error: bool,
}

#[allow(unused)]
impl Lox {
    fn new() -> Self {
        Self {
            had_error: false,
            had_runtime_error: false,
        }
    }

    fn run(&self, tokens_instead_of_source_code: Vec<Tk>) {
        let mut p = Parser::new(tokens_instead_of_source_code);
        p.parse();
        //*  Stop if there was a syntax error.
        if self.had_error {}
    }
}

fn main() {
    /*
     * A parser really has two jobs:
     *
     * - Given a valid sequence of tokens, produce a corresponding syntax tree.
     *
     * - Given an invalid sequence of tokens, detect any errors and tell
     * the user about their mistakes.
     */
    //? When the user doesn’t realize the syntax is wrong,
    //? it is up to the parser to help guide them back onto the right path.
    //? The parser can’t read your mind❓
    //? With the way things are going in machine learning these days,
    //? who knows what the future will bring?
    let expression = Expr::binary_op(
        Expr::unary_op(Tk::Sub, Expr::literal(123)),
        Tk::Mul,
        Expr::grouping(Expr::literal(45.65)),
    );
    println!("{:?}", expression.to_string());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let expression = Expr::binary_op(
            Expr::unary_op(Tk::Sub, Expr::literal(123)),
            Tk::Mul,
            Expr::grouping(Expr::literal(45.65)),
        );
        assert_eq!(
            expression.to_string(),
            "(* (-123) (group 45.65))".to_string()
        );
    }

    //* ->  1 - (2 * 3) < 4 == false
    #[test]
    fn parser_works_with_err() {
        let tokens = vec![
            Tk::Num(1),
            Tk::Sub,
            Tk::Lpar,
            Tk::Class,
            Tk::Mul,
            Tk::Num(3),
            Tk::Rpar,
            Tk::LT,
            Tk::Num(4),
            Tk::EQ,
            Tk::False,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);

        let expression = parser.parse();

        assert_eq!(
            //*  "(- 1 (* (group ❌ algún error!) 3))"
            expression.to_string(),
            "(== (< (- 1 (group (* 2 3))) 4) false)".to_string()
        );
    }

    //* ->  1 - (2 * 3( < 4 == false
    #[test]
    fn parser_works() {
        let tokens = vec![
            Tk::Num(1),
            Tk::Sub,
            Tk::Lpar,
            Tk::Num(2),
            Tk::Mul,
            Tk::Num(3),
            Tk::Lpar,
            Tk::LT,
            Tk::Num(4),
            Tk::EQ,
            Tk::False,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);

        let expression = match parser.expression() {
            Ok(exp) => exp.to_string(),
            Err(_) => todo!(),
        };

        assert_eq!(
            expression,
            "(== (< (- 1 (group (* 2 3))) 4) false)".to_string()
        );
    }

    //* ->  -123 * (45.65)
    #[test]
    fn parser_works_2() {
        let tokens = vec![
            Tk::Sub,
            Tk::Num(123),
            Tk::Mul,
            Tk::Lpar,
            Tk::Float(45.65),
            Tk::Rpar,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);

        let expression = match parser.expression() {
            Ok(exp) => exp.to_string(),
            Err(_) => todo!(),
        };
        assert_eq!(expression, "(* (-123) (group 45.65))".to_string());
    }
}
