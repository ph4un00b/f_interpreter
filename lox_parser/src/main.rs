use std::fmt::{self, Display};

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
        }

        Ok(())
    }
}

#[allow(dead_code)]
struct Parser {
    tokens: Vec<Tk>,
    current_position: usize,
    current_token: Tk,
    // next_token: Tk,
    prev_token: Tk,
}

#[allow(dead_code)]
impl Parser {
    fn new(tokens: Vec<Tk>) -> Self {
        let mut p = Parser {
            tokens,
            current_position: 0,
            current_token: Tk::Default,
            // next_token: Tk::DEFAULT,
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
    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while self.current_token == Tk::EQ {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.comparison();
            expr = Expr::binary_op(expr, operator, right);
        }
        expr
    }

    // * comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while self.current_token == Tk::LT {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.term();
            expr = Expr::binary_op(expr, operator, right);
        }
        expr
    }

    fn init_tokens(&mut self) {
        self.current_token = self.tokens[self.current_position].clone();
        // self.next_token = self.tokens[self.current_position + 1].clone();
        self.prev_token = Tk::Default;
    }

    fn consume_token(&mut self) {
        self.current_position += 1;
        self.current_token = self.tokens[self.current_position].clone();
        // self.next_token = if self.current_token == Tk::EOF {
        //     self.tokens[self.current_position - 1].clone()
        // } else {
        //     self.tokens[self.current_position + 1].clone()
        // };
        self.prev_token = self.tokens[self.current_position - 1].clone();

        // println!("consumed {:?}", self.current_token);
        // println!("next {:?}", self.next_token);
        // println!("prev {:?}", self.prev_token);
    }

    // * term       = _{ factor ~ ((sub | add) ~ factor)* }
    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while self.current_token == Tk::Sub || self.current_token == Tk::Mul {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.factor();
            expr = Expr::binary_op(expr, operator, right);
        }
        expr
    }

    // * factor     = _{ unary ~ ((div | mul) ~ unary)* }
    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        while self.current_token == Tk::Mul {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.unary();
            expr = Expr::binary_op(expr, operator, right);
        }
        expr
    }

    // * unary      = _{ (bang | neg) ~ unary | primary }
    fn unary(&mut self) -> Expr {
        if self.current_token == Tk::Bang || self.current_token == Tk::Sub {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.unary();
            return Expr::unary_op(operator, right);
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        match self.current_token {
            Tk::False => {
                self.consume_token();
                Expr::literal("false")
            }
            Tk::Num(val) => {
                self.consume_token();
                Expr::literal(val)
            }
            Tk::Float(val) => {
                self.consume_token();
                Expr::literal(val)
            }

            Tk::Lpar => {
                self.consume_token();
                let expr = self.expression();
                // self.consume_token(); //? "Expect ')' after expression.");
                if self.current_token != Tk::Rpar {
                    panic!("❌no no!");
                }
                self.consume_token();
                Expr::grouping(expr)
            }
            _ => unreachable!(),
        }
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }
}

fn main() {
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
            Tk::Rpar,
            Tk::LT,
            Tk::Num(4),
            Tk::EQ,
            Tk::False,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        assert_eq!(
            parser.expression().to_string(),
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
        assert_eq!(
            parser.expression().to_string(),
            "(* (-123) (group 45.65))".to_string()
        );
    }
}
