use crate::{
    ast::{ToLiteral, P},
    ast_expression::Expr,
    parser::Parsing,
};

pub struct InfixExpr;
impl InfixExpr {
    pub(crate) fn literal(left: &Expr, op: &crate::scanner::Tk, right: &Expr) -> String {
        format!("{}{op}{}", left.to_literal(), right.to_literal())
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        left: &Expr,
        op: &crate::scanner::Tk,
        right: &Expr,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "(")?;
        write!(f, "{left}")?;
        write!(f, " {op} ")?;
        write!(f, "{right}")?;
        write!(f, ")")?;
        Ok(())
    }

    pub(crate) fn parse(p: &mut crate::parser::Parser, left: Expr) -> Option<Expr> {
        let op = p.current_token.to_owned();
        let precedence = P::from(&p.current_token);
        p.next_token();
        p.parse_expression(precedence).map(|expr| Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(expr),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{ToLiteral, V},
        ast_expression::Expr,
        ast_statements::Statement,
        lexer::Lexer,
        parser::{Errors, Parser, Parsing},
    };

    #[test]
    fn test_infix_precedence() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (_index, test) in tests.iter().enumerate() {
            let (input, expected) = *test;
            let program = setup(input);
            assert_eq!(program.to_string(), format!("{expected}"));
        }
    }

    #[test]
    fn test_infix_expressions() {
        let tests = vec![
            ("5 + 5;", 5i64, "+", 5i64),
            ("5 - 5;", 5i64, "-", 5i64),
            ("5 * 5;", 5i64, "*", 5i64),
            ("5 / 5;", 5i64, "/", 5i64),
            ("5 > 5;", 5i64, ">", 5i64),
            ("5 < 5;", 5i64, "<", 5i64),
            ("5 == 5;", 5i64, "==", 5i64),
            ("5 != 5;", 5i64, "!=", 5i64),
        ];

        for (_index, test) in tests.iter().enumerate() {
            let (input, expected_left, expected_op, expected_right) = *test;
            let program = setup(input);
            assert_eq!(
                program.len(),
                1,
                "program has not enough statements. got {}",
                program.len()
            );

            for (_i, stmt) in program.enumerate() {
                println!("> {stmt}");
                let (left, op, right) = match stmt {
                    Statement::Expr {
                        first_token: _,
                        expr,
                    } => {
                        println!(">> {}", expr.to_literal());
                        match expr {
                            Expr::Binary { op, right, left } => (left, op, right),
                            _ => unreachable!("stmt is not ast.InfixExpression. got {expr:?}"),
                        }
                    }
                    _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
                };
                assert_eq!(
                    &op.to_string(),
                    expected_op,
                    "exp.Operator is not '{expected_op}'. got {op}",
                );
                let left_literal = left.to_literal();
                let left_val = match *left {
                    Expr::Literal {
                        token: _,
                        value: V::I64(val),
                    } => val,
                    _ => unreachable!("not *ast.IntegerLiteral. got {left}"),
                };
                assert_eq!(
                    left_val, expected_left,
                    "Value not {expected_left}. got {left_val}"
                );
                assert_eq!(
                    left_literal,
                    expected_left.to_string(),
                    "TokenLiteral not {expected_left}. got {left_literal}"
                );
                let right_literal = right.to_literal();
                let right_val = match *left {
                    Expr::Literal {
                        token: _,
                        value: V::I64(val),
                    } => val,
                    _ => unreachable!("not *ast.IntegerLiteral. got {left}"),
                };
                assert_eq!(
                    right_val, expected_right,
                    "Value not {expected_right}. got {right_val}"
                );
                assert_eq!(
                    left_literal,
                    expected_right.to_string(),
                    "TokenLiteral not {expected_right}. got {right_literal}"
                );
            }
        }
    }

    fn setup(input: &str) -> crate::program_node::Program {
        let lex = Lexer::new(input.into());
        let mut p = Parser::new(lex);
        let program = p.parse_program();
        for err in p.errors() {
            println!("🎈 {err}");
        }
        assert_eq!(p.errors().len(), 0);
        program
    }
}
