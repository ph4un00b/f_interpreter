use std::fmt::{self, Display};

fn string_err(expected_token: Tk, token: Tk, context_message: &str) -> String {
    if token == Tk::End {
        format!(
            "❌ expected next token to be {:?} at end, {}, got {:?} instead",
            expected_token, context_message, token
        )
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
            Expr::None => unreachable!(),
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
        if let Ok(expression) = self.expression() {
            expression
        } else {
            //? aquí podríamos usar un dummy expression
            //? o quizá usar un OPTION
            //? por lo mientras usaremos un dummy EXPR
            //? despues vemos como se ve el api final 🤔
            //? hay ue ver si hay mucho kilombo con los matches
            Expr::None
        }
    }

    /*
     * different ways to error handling in rust 😏
     * the methods below will have the checked ✅ strategy:
     *
     * ✅ matching
     * ✅ you can use String as Errors	   Result<(), String>
     * errors #2	                        unwrap_or_else
     * errors #3	                        yeet + yeet crate
     * shortcut error	                    using expect("custom error")
     * question mark operator	            maybe_Error()?.chained_maybe_error()?
     * errors #4	                        map_error
     * Enums as Errors for custom messaging	Result<(), MyError>
     * thiserror                            crate
     * anyhow	                            crate + trait objects
     * eyre                                 crate
     */
    fn expression(&mut self) -> Result<Expr, String> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = match self.comparison() {
            Ok(exp) => exp,
            Err(err) => return Err(err),
        };
        while self.current_token == Tk::EQ {
            self.consume_token();
            let operator = self.prev_token.clone();
            expr = match self.comparison() {
                Ok(right) => Expr::binary_op(expr, operator, right),
                Err(err) => return Err(err),
            };
        }
        Ok(expr)
    }

    // * comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = match self.term() {
            Ok(exp) => exp,
            Err(err) => return Err(err),
        };
        while self.current_token == Tk::LT {
            self.consume_token();
            let operator = self.prev_token.clone();
            expr = match self.term() {
                Ok(right) => Expr::binary_op(expr, operator, right),
                Err(err) => return Err(err),
            };
        }
        Ok(expr)
    }

    // * term       = _{ factor ~ ((sub | add) ~ factor)* }
    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = match self.factor() {
            Ok(exp) => exp,
            //? esto según yo es la misma estructura que abajo
            //? en efecto
            Err(err) => return Err(err),
        };

        while self.current_token == Tk::Sub || self.current_token == Tk::Mul {
            self.consume_token();
            let operator = self.prev_token.clone();
            expr = match self.factor() {
                Ok(right) => Expr::binary_op(expr, operator, right),
                Err(err) => return Err(err),
            };
        }
        Ok(expr)
    }

    // * factor     = _{ unary ~ ((div | mul) ~ unary)* }
    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = match self.unary() {
            Ok(exp) => exp,
            //? se esta poniendo recursivo esto ( •_•)>⌐■-■
            //? phos: a veces cuando no se puede inferir el
            //? tipo puedes especificarle el tipo de Result que devuelves
            //? Ok::<_, String> o Err::<Expr, _>

            //? pense que el return esta implícito
            //? al devolver el err en el match.
            //? pero sigue siendo asignación❗
            //? hay que hacerlo 😏 explicito.
            Err(err) => return Err(err),
        };
        while self.current_token == Tk::Mul {
            self.consume_token();
            let operator = self.prev_token.clone();

            expr = match self.unary() {
                Ok(right) => Expr::binary_op(expr, operator, right),
                //? pense que el return esta implícito
                //? al devolver el err en el match.
                //? pero sigue siendo asignación❗
                //? hay que hacerlo 😏 explicito.
                Err(err) => return Err(err),
            };
        }
        Ok(expr)
    }

    // * unary      = _{ (bang | neg) ~ unary | primary }
    fn unary(&mut self) -> Result<Expr, String> {
        if self.current_token == Tk::Bang || self.current_token == Tk::Sub {
            self.consume_token();
            let operator = self.prev_token.clone();

            match self.unary() {
                Ok(right) => return Ok(Expr::unary_op(operator, right)),
                // todo: por que no puede inferir el type ❓
                //* aquí parece que se confunde si no retornamos el error
                //* como que trata de inferir una asignación.
                //? Err(err) => Err::<Expr, String>(err) <- esto salvó cuando asumía asignación el compiler
                //? aca importante no olvidar retornar el error❗👀
                Err(err) => return Err(err),
            };
        }
        self.primary()
    }

    // todo: Add support for comma expressions
    // todo: add support for the C-style conditional or “ternary” operator ?:.
    // What precedence level is allowed between the ? and :?
    // Is the whole operator left-associative or right-associative?
    // todo: Add error productions to handle each binary operator appearing without a left-hand operand.
    // In other words, detect a binary operator appearing at the beginning of an expression
    fn primary(&mut self) -> Result<Expr, String> {
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
                let expr = match self.expression() {
                    Ok(exp) => exp,
                    Err(err) => return Err(err),
                };
                let expected_token = Tk::Rpar;
                //* verifying token
                if self.current_token != expected_token {
                    lox_report_error(
                        expected_token,
                        self.current_token.clone(),
                        "')' after expression",
                    );
                }
                self.consume_token();
                Ok(Expr::grouping(expr))
            }
            _ => Err(string_err(Tk::Default, Tk::Default, "Expect expression.")),
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
