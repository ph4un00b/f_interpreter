use crate::{ast::V, ast_expression::Expr, parser::Parser, scanner::Tk};

pub struct BooleanExpr;
impl BooleanExpr {
    pub(crate) fn parse(p: &Parser) -> Option<crate::ast_expression::Expr> {
        Some(Expr::Literal {
            token: p.current_token.to_owned(),
            value: V::Bool(p.current_token == Tk::True),
        })
    }

    pub(crate) fn name() -> String {
        String::from("bool")
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        val: bool,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{val}")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Name, ToLiteral},
        ast_statements::Statement,
        parser_test::parse_program,
        scanner::Tk,
    };

    #[test]
    fn test_true() {
        let input = r#"
        true;
    "#;
        let program = parse_program(input);
        assert_eq!(
            program.len(),
            1,
            "program has not enough statements. got {}",
            program.len()
        );

        for (_i, stmt) in program.enumerate() {
            match stmt {
                Statement::Expr { first_token, expr } => {
                    assert_eq!(expr.name(), "bool");
                    assert_eq!(expr.to_literal(), "true");
                    assert_eq!(expr.to_string(), "true");
                    assert_eq!(
                        first_token,
                        Tk::True,
                        "Token not '{:?}'. got {:?}",
                        Tk::True,
                        first_token
                    );
                }
                _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
            }
        }
    }

    #[test]
    fn test_false() {
        let input = r#"
        false;
    "#;
        let program = parse_program(input);
        assert_eq!(
            program.len(),
            1,
            "program has not enough statements. got {}",
            program.len()
        );

        for (_i, stmt) in program.enumerate() {
            match stmt {
                Statement::Expr { first_token, expr } => {
                    assert_eq!(expr.name(), "bool");
                    assert_eq!(expr.to_literal(), "false");
                    assert_eq!(expr.to_string(), "false");
                    assert_eq!(
                        first_token,
                        Tk::False,
                        "Token not '{:?}'. got {:?}",
                        Tk::False,
                        first_token
                    );
                }
                _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
            }
        }
    }
}
