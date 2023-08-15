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
        ast_expression::Expr,
        ast_statements::Statement,
        parser_test::{assert_literal_expr, parse_program},
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
            let program = parse_program(input);
            assert_eq!(program.to_string(), format!("{expected}"));
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
            // (
            //     "alice * bob;",
            //     V::String("alice".into()),
            //     "+",
            //     V::String("bob".into()),
            // ),
        ];

        for (_index, test) in tests.iter().enumerate() {
            let (input, expected_left, expected_op, expected_right) = test;
            let program = parse_program(input);
            assert_eq!(
                program.len(),
                1,
                "program has not enough statements. got {}",
                program.len()
            );

            for stmt in program {
                assert_infix_expr(
                    stmt,
                    expected_left.clone(),
                    expected_op,
                    expected_right.clone(),
                );
            }
        }
    }

    fn assert_infix_expr(stmt: Statement, expected_left: V, expected_op: &str, expected_right: V) {
        println!("> {stmt}");
        if let Statement::Expr {
            first_token: _,
            expr: Expr::Binary { op, right, left },
        } = stmt
        {
            assert_literal_expr(left, expected_left);
            assert_eq!(
                &op.to_string(),
                expected_op,
                "exp.Operator is not '{expected_op}'. got {op}",
            );
            assert_literal_expr(right, expected_right);
        } else {
            unreachable!("not *ast.Statement::Expr. got {stmt:?}")
        };
    }
}
