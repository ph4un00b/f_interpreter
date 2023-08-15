use crate::{ast_expression::Expr, scanner::Tk};

/*
 * But the identifier in a let statement doesn’t produce
 * a value, right? So why is it an Expression?
 *
 * ✅ It’s to keep things simple.
 */
pub struct IdentExpr;

impl IdentExpr {
    pub fn name() -> String {
        String::from("id")
    }

    pub fn literal(name: &Tk) -> String {
        String::from(name)
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        name: &Tk,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{}", String::from(name))?;
        Ok(())
    }

    pub(crate) fn parse(p: &crate::parser::Parser) -> Option<Expr> {
        Some(Expr::Ident(p.current_token.clone()))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast_statements::Statement,
        parser_test::{assert_identifier, parse_program},
        scanner::Tk,
    };

    #[test]
    fn test_identifier() {
        let input = r#"
        foobar;
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
                    assert_identifier(expr, "id", "foobar", "foobar");
                    assert_eq!(
                        first_token,
                        Tk::Ident("foobar".into(), 2),
                        "Token not '{:?}'. got {:?}",
                        Tk::Ident("foobar".into(), 2),
                        first_token
                    );
                }
                _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
            }
        }
    }
}
