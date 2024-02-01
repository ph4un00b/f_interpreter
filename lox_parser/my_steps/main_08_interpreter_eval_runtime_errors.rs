use std::fmt::{self, Display};

#[allow(unused)]
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

fn report_err(expected_token: Tk, token: Tk, context_message: &str) {
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
#[derive(Debug, PartialEq)]
#[allow(dead_code)]
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
    Literal(V),
    Grouping(Box<Expr>),
    None,
}

#[derive(Debug, PartialEq)]
enum V {
    I32(i32),
    F64(f64),
    String(String),
    Bool(bool),
}

impl fmt::Display for V {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            V::I32(val) => write!(f, "{}", val)?,
            V::F64(val) => write!(f, "{}", val)?,
            V::String(val) => write!(f, "{}", val)?,
            V::Bool(val) => write!(f, "{}", val)?,
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

impl ToLiteral<bool> for Expr {
    fn literal(val: bool) -> Self {
        Expr::Literal(V::Bool(val))
    }
}

impl ToLiteral<i32> for Expr {
    fn literal(val: i32) -> Self {
        Expr::Literal(V::I32(val))
    }
}

impl ToLiteral<String> for Expr {
    fn literal(val: String) -> Self {
        Expr::Literal(V::String(val))
    }
}

impl ToLiteral<&str> for Expr {
    fn literal(val: &str) -> Self {
        Expr::Literal(V::String(val.to_string()))
    }
}

impl ToLiteral<f64> for Expr {
    fn literal(val: f64) -> Self {
        Expr::Literal(V::F64(val))
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

#[derive(Debug, PartialEq)]
enum Statement {
    None,
    Print(Expr),
    Expr(Expr),
}

#[allow(dead_code)]
struct Parser {
    tokens: Vec<Tk>,
    current_position: usize,
    current_token: Tk,
    prev_token: Tk,
}

#[allow(dead_code)]
struct Interpreter {
    runtime_error: bool,
}

#[allow(dead_code)]
impl Interpreter {
    fn new() -> Self {
        Self {
            runtime_error: false,
        }
    }

    fn eval(&self, statements: Vec<Statement>) {
        for state in statements {
            self.execute(state);
        }
    }
    fn eval_statement(&self) {}

    fn execute(&self, state: Statement) {
        match state {
            Statement::None => todo!(),
            Statement::Print(expr) => {
                let value = self.eval_expr(expr);
                match value {
                    Ok(val) => println!("🎈 {val}"),
                    Err(err) => match err {
                        RE::MustBeNumber(_) => todo!(),
                        RE::NotNumber(_, _, _) => todo!(),
                        RE::MustBeBoolean(_) => todo!(),
                    },
                }

                //? refactor on complex matches❗
                //? Expr::Literal(value) => match value {
                //?     Value::I32(data) => {
                //?         println!("🎈 {data}")
                //?     },
                //?     Value::F64(_) => todo!(),
                //?     Value::String(_) => todo!(),
                //? },
                //? _ => unreachable!(),
            }
            Statement::Expr(_) => todo!(),
        }
    }

    fn eval_expr(&self, expr: Expr) -> RValue {
        println!("{:?}", expr);
        match expr {
            Expr::Unary { operator, right } => {
                /*
                 * First, we evaluate the operand expression.
                 * Then we apply the unary operator itself to
                 * the result of that. There are two different
                 * unary expressions, identified by the type of
                 * the operator token.
                 */
                let right = self.eval_expr(*right)?;
                match (operator, right) {
                    (Tk::Sub, V::I32(r)) => Ok(V::I32(-r)),
                    (Tk::Sub, V::F64(r)) => Ok(V::F64(-r)),
                    (Tk::Sub, value) => Err(RE::MustBeNumber(value)),
                    (Tk::Bang, V::Bool(false)) => Ok(V::Bool(true)),
                    (Tk::Bang, V::Bool(true)) => Ok(V::Bool(false)),
                    (Tk::Bang, value) => Err(RE::MustBeBoolean(value)),
                    /*
                     * You’re probably wondering what happens
                     * if the cast fails❓.
                     * Fear not, we’ll get into that soon.
                     */
                    _ => unreachable!(),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                /*
                 * Did you notice we pinned down a subtle corner of the
                 * language semantics here? In a binary expression,
                 * we evaluate the operands in left-to-right order.
                 * If those operands have side effects, that choice
                 * is user visible, so this isn’t simply an implementation
                 * detail.
                 */
                //? Another subtle semantic choice: We evaluate both
                //? operands before checking the type of either.
                //? Imagine we have a function say() that prints its
                //? argument then returns it.
                //? We could have instead specified that the left operand
                //? is checked before even evaluating the right.
                let left = self.eval_expr(*left)?;
                println!("left: {:?}", left);
                let right = self.eval_expr(*right)?;
                println!("right: {:?}", right);
                match (left, operator, right) {
                    // todo: +, /
                    /*
                     * The + operator can also be used to concatenate two strings.
                     * To handle that, we don’t just assume the operands are
                     * a certain type and cast them, we dynamically check the
                     * type and choose the appropriate operation.
                     */
                    (V::I32(l), Tk::Sub, V::I32(r)) => Ok(V::I32(l - r)),
                    (V::F64(l), Tk::Sub, V::F64(r)) => Ok(V::F64(l - r)),
                    (l, Tk::Sub, r) => Err(RE::NotNumber(l, Tk::Sub, r)),
                    (V::I32(l), Tk::Mul, V::I32(r)) => Ok(V::I32(l * r)),
                    (V::F64(l), Tk::Mul, V::F64(r)) => Ok(V::F64(l * r)),
                    (l, Tk::Mul, r) => Err(RE::NotNumber(l, Tk::Mul, r)),
                    // todo: What happens right now if you divide a number by zero? What do you think should happen?
                    /*
                     * comparisons:
                     * They are basically the same as arithmetic.
                     * The only difference is that where the arithmetic
                     * operators produce a value whose type is the same as \
                     * the operands (numbers or strings), the comparison
                     * operators always produce a Boolean.
                     */
                    (V::I32(l), Tk::EQ, V::I32(r)) => Ok(V::Bool(l.eq(&r))),
                    (V::F64(l), Tk::EQ, V::F64(r)) => Ok(V::Bool(l.eq(&r))),
                    (l, Tk::EQ, r) => Err(RE::NotNumber(l, Tk::EQ, r)),
                    (V::I32(l), Tk::LT, V::I32(r)) => Ok(V::Bool(l.lt(&r))),
                    (V::F64(l), Tk::LT, V::F64(r)) => Ok(V::Bool(l.lt(&r))),
                    (l, Tk::LT, r) => Err(RE::NotNumber(l, Tk::LT, r)),
                    /*
                     * Allowing comparisons on types other than numbers
                     * could be useful. The operators might have a reasonable
                     * interpretation for strings. Even comparisons among
                     * mixed types, like 3 < "pancake" could be handy to
                     * enable things like ordered collections of heterogeneous
                     * types. Or it could simply lead to bugs and confusion.
                     */
                    // todo: + handle the case for string concatenation "'a' + 'a'"❗
                    /*
                     * Many languages define + such that if either operand
                     * is a string, the other is converted to a string and
                     * the results are then concatenated. For example,
                     * "scone" + 4 would yield scone4.
                     *
                     * como lo vió en javascript 👀❗
                     */
                    _ => unreachable!(),
                }
            }
            Expr::Literal(value) => Ok(value),
            Expr::Grouping(expr) => self.eval_expr(*expr),
            _ => unreachable!(),
        }
    }
}

enum ParserAy {
    BadExpression(Tk),
}

enum RE {
    MustBeNumber(V),
    NotNumber(V, Tk, V),
    MustBeBoolean(V),
}
use std::result::Result as StdResult;
type Result<T> = StdResult<T, ParserAy>;
type RStatement = StdResult<Statement, ParserAy>;
type RValue = StdResult<V, RE>;

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
        println!("tk: {:?}", self.current_token);
    }
    /*
     * GRAMAR:
     *
     * program        → statement* EOF
     * statement      → exprStmt  | printStmt
     * exprStmt       → expression ";"
     * printStmt      → "print" expression ";"
     */

    fn parse(&mut self) -> Vec<Statement> {
        /*
         * Now that our grammar has the correct
         * starting rule, program, we can turn parse()
         * into the real deal.
         */
        let mut result = vec![];

        while self.current_token != Tk::End {
            let statement = self.statement().unwrap_or_else(|err| {
                match err {
                    ParserAy::BadExpression(bad_token) => {
                        println!(
                            "❌ desde parse!, expected expression got {:?}😱❗",
                            bad_token
                        )
                    }
                };

                Statement::None
            });
            result.push(statement);
            self.consume_token();
        }

        result
    }

    fn statement(&mut self) -> RStatement {
        /*
         * When a syntax error does occur, this method returns null.
         * That’s OK. The parser promises not to crash or hang on
         * invalid syntax, but it doesn’t promise to return a
         * usable syntax tree if an error is found. As soon as
         * the parser reports an error, hadError gets set, and
         * subsequent phases are skipped.
         */

        let stt = match self.current_token {
            Tk::Default => todo!(),
            Tk::Print => self.print_statement()?,
            Tk::Num(_) => todo!(),
            Tk::Float(_) => todo!(),
            _ => self.expr_statement()?,
        };

        Ok(stt)
    }

    // * exp =  { eq }
    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    // * eq  = _{ comparison ~ (("!=" | "==") ~ comparison)* }
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
                Ok(Expr::literal(false))
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
                    report_err(Tk::Rpar, self.current_token.clone(), "')' after EXPRESSION");
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

    fn print_statement(&mut self) -> RStatement {
        self.consume_token();
        // Expr value = expression();
        let value = self.expression()?;
        // consume(SEMICOLON, "Expect ';' after value.");
        if self.current_token != Tk::Semi {
            report_err(
                Tk::Semi,
                self.current_token.clone(),
                "👀 expected ';' after value",
            );
        }
        // return new Stmt.Print(value);
        Ok(Statement::Print(value))
    }

    fn expr_statement(&mut self) -> RStatement {
        // Expr expr = expression();
        let value = self.expression()?;
        // consume(SEMICOLON, "Expect ';' after expression.");
        if self.current_token != Tk::Rpar {
            report_err(
                Tk::Semi,
                self.current_token.clone(),
                "👀 expected ';' after expression",
            );
        }
        // return new Stmt.Expression(expr);
        Ok(Statement::Expr(value))
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

    //* put !42 ;
    //* "throws" not a boolean❗
    #[test]
    #[should_panic = "not yet implemented"]
    fn test_runtime_error_on_unary_bang() {
        let tokens = vec![Tk::Print, Tk::Bang, Tk::Num(42), Tk::Semi, Tk::End];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        println!("{:?}", statements);
        let inter = Interpreter::new();
        inter.eval(statements);

        // todo: assert stdout
        //? assert_eq!(expr, Statement::Print(Expr::Literal(Value::I32(1))));
    }

    //* ->  !false
    #[test]
    fn test_bool_expressions() {
        let tokens = vec![Tk::Bang, Tk::False, Tk::End];
        let mut parser = Parser::new(tokens);
        let expression = parser.expression().unwrap_or_else(|_| Expr::None);

        assert_eq!(
            expression,
            Expr::Unary {
                operator: Tk::Bang,
                right: Box::new(Expr::Literal(V::Bool(false)))
            }
        );
    }

    //* put 2-3 ;
    #[test]
    fn test_run_interpreter() {
        let tokens = vec![
            Tk::Print,
            Tk::Num(2),
            Tk::Sub,
            Tk::Num(3),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let inter = Interpreter::new();
        inter.eval(statements);

        // todo: assert stdout
        //? assert_eq!(expr, Statement::Print(Expr::Literal(Value::I32(1))));
    }

    //* put 2 - false ;
    //* "throws" not a number❗
    #[test]
    #[should_panic = "not yet implemented"]
    fn test_runtime_error() {
        let tokens = vec![Tk::Print, Tk::Num(2), Tk::Sub, Tk::False, Tk::Semi, Tk::End];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        println!("{:?}", statements);
        let inter = Interpreter::new();
        inter.eval(statements);

        // todo: assert stdout
        //? assert_eq!(expr, Statement::Print(Expr::Literal(Value::I32(1))));
    }

    //* put 1;
    #[test]
    fn test_interpreter() {
        let tokens = vec![Tk::Print, Tk::Num(1), Tk::Semi, Tk::End];
        let mut parser = Parser::new(tokens);
        let expression = parser.parse();

        for expr in expression {
            assert_eq!(expr, Statement::Print(Expr::Literal(V::I32(1))));
        }
    }

    //* put 1;
    #[test]
    fn test_print() {
        let tokens = vec![Tk::Print, Tk::Num(1), Tk::Semi, Tk::End];
        let mut parser = Parser::new(tokens);
        let expression = parser.parse();

        for expr in expression {
            assert_eq!(expr, Statement::Print(Expr::Literal(V::I32(1))));
        }
    }

    //* ->  1 - (2 * 3) < 4 == false
    #[test]
    #[should_panic]
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
        let expression = parser.expression().unwrap_or_else(|_| Expr::None);

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
        let expression = parser.expression().unwrap_or_else(|_| Expr::None);

        assert_eq!(
            expression.to_string(),
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
        let expression = parser.expression().unwrap_or_else(|_| Expr::None);

        assert_eq!(
            expression.to_string(),
            "(* (-123) (group 45.65))".to_string()
        );
    }
}
