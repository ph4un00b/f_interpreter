use crate::{ast::P, ast_expression::Expr, parser::Parsing};

pub struct PrefixExpr;
impl PrefixExpr {
    pub(crate) fn literal(op: &crate::scanner::Tk) -> String {
        op.to_string()
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        op: &crate::scanner::Tk,
        right: &Expr,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "(")?;
        write!(f, "{op}")?;
        write!(f, "{right}")?;
        write!(f, ")")?;
        Ok(())
    }

    pub(crate) fn parse(p: &mut crate::parser::Parser) -> Option<Expr> {
        let op = p.current_token.to_owned();
        p.next_token();
        p.parse_expression(P::Prefix).map(|expr| Expr::Unary {
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
        parser_test::{assert_literal_boxed_expression, parse_program},
    };

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            ("!true;", "!", V::Bool(true)),
            ("!false;", "!", V::Bool(false)),
        ];

        for (input, expected_op, expected_left) in tests {
            let program = parse_program(input);
            assert_eq!(
                program.len(),
                1,
                "program has not enough statements. got {}",
                program.len()
            );

            for stmt in program {
                assert_prefix_expr(stmt, expected_op, &expected_left);
            }
        }
    }

    #[test]
    fn test_prefix_expressions() {
        let tests = vec![("!5", "!", V::I64(5i64)), ("-15", "-", V::I64(15i64))];

        for (_index, test) in tests.iter().enumerate() {
            let (test_input, expected_op, expected_value) = test;
            let program = parse_program(test_input);
            assert_eq!(
                program.len(),
                1,
                "program has not enough statements. got {}",
                program.len()
            );

            for (_i, stmt) in program.enumerate() {
                assert_prefix_expr(stmt, expected_op, expected_value);
            }
        }
    }

    fn assert_prefix_expr(stmt: Statement, expected_op: &str, expected_value: &V) {
        println!("> {stmt:?}");
        if let Statement::Expr {
            first_token: _,
            expr: Expr::Unary { op, right },
        } = stmt
        {
            assert_eq!(
                &op.to_string(),
                expected_op,
                "exp.Operator is not '{expected_op}'. got {op}",
            );
            assert_literal_boxed_expression(right, expected_value.clone());
        } else {
            unreachable!("not *ast.Statement::Expr. got {stmt:?}")
        }
    }
}
