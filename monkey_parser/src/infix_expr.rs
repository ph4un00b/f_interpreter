use crate::{
    ast::{ToLiteral, P},
    ast_expression::Expr,
    parser::Parsing,
};

pub struct InfixExpr;
impl InfixExpr {
    pub(crate) fn literal(left: &Expr, op: &crate::scanner::Tk, right: &Expr) -> String {
        format!("{} {op} {}", left.to_literal(), right.to_literal())
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
        ast::V,
        parser_test::{assert_infix_stmt, parse_program},
    };

    #[test]
    fn test_function_call_precedence() {
        let tests = [
            ("a + add(b * c) + d", "((a + #add (b * c)) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "#add a b 1 (2 * 3) (4 + 5) #add 6 (7 * 8)",
            ),
            (
                "add(a + b + c * d / f + g)",
                "#add (((a + b) + ((c * d) / f)) + g)",
            ),
        ];

        for (input, expected) in tests {
            let program = parse_program(input);
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            ("true == true", V::Bool(true), "==", V::Bool(true)),
            ("true != false", V::Bool(true), "!=", V::Bool(false)),
            ("false == false", V::Bool(false), "==", V::Bool(false)),
        ];

        for (input, expected_left, expected_op, expected_right) in tests {
            let program = parse_program(input);
            assert_eq!(
                program.len(),
                1,
                "program has not enough statements. got {}",
                program.len()
            );

            for stmt in program {
                assert_infix_stmt(
                    stmt,
                    expected_left.clone(),
                    expected_op,
                    expected_right.clone(),
                );
            }
        }
    }

    #[test]
    fn test_boolean_precedence() {
        let tests = [
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
        ];

        for (input, expected) in tests {
            let program = parse_program(input);
            assert_eq!(program.to_string(), expected);
        }
    }

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

        for (input, expected) in tests {
            let program = parse_program(input);
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn test_infix_expressions() {
        let tests = vec![
            ("5 + 5;", V::I64(5i64), "+", V::I64(5i64)),
            ("5 - 5;", V::I64(5i64), "-", V::I64(5i64)),
            ("5 * 5;", V::I64(5i64), "*", V::I64(5i64)),
            ("5 / 5;", V::I64(5i64), "/", V::I64(5i64)),
            ("5 > 5;", V::I64(5i64), ">", V::I64(5i64)),
            ("5 < 5;", V::I64(5i64), "<", V::I64(5i64)),
            ("5 == 5;", V::I64(5i64), "==", V::I64(5i64)),
            ("5 != 5;", V::I64(5i64), "!=", V::I64(5i64)),
            ("5 + 10;", V::I64(5i64), "+", V::I64(10i64)),
        ];

        for (input, expected_left, expected_op, expected_right) in tests {
            let program = parse_program(input);
            assert_eq!(
                program.len(),
                1,
                "program has not enough statements. got {}",
                program.len()
            );

            for stmt in program {
                assert_infix_stmt(
                    stmt,
                    expected_left.clone(),
                    expected_op,
                    expected_right.clone(),
                );
            }
        }
    }
}
