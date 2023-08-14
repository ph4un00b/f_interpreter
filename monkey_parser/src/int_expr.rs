use crate::{ast::V, ast_expression::Expr, parser::Errors, scanner::Tk};

pub struct IntegerExpr;

impl IntegerExpr {
    pub fn literal(token: &Tk) -> String {
        String::from(token)
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        val: &i64,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{val}")?;
        Ok(())
    }

    pub(crate) fn parse(p: &mut crate::parser::Parser, value: &str) -> Option<Expr> {
        let val = match value.parse::<i64>() {
            Ok(v) => v,
            Err(e) => {
                p.append_error(e.to_string());
                return None;
            }
        };
        Some(Expr::Literal {
            token: p.current_token.to_owned(),
            value: V::I64(val),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::ToLiteral,
        ast_statements::Statement,
        lexer::Lexer,
        parser::{Errors, Parser, Parsing},
        scanner::Tk,
    };

    #[test]
    fn test_identifier() {
        let input = r#"
        5;
    "#;
        let lex = Lexer::new(input.into());
        let mut p = Parser::new(lex);
        let program = p.parse_program();
        for err in p.errors() {
            println!("🎈 {err}");
        }
        assert_eq!(p.errors().len(), 0);
        assert_eq!(
            program.len(),
            1,
            "program has not enough statements. got {}",
            program.len()
        );

        for (_i, stmt) in program.enumerate() {
            println!("> {stmt}");
            match stmt.clone() {
                Statement::Expr {
                    first_token,
                    expr: _,
                } => {
                    assert_eq!(
                        first_token,
                        Tk::Num("5".into(), 2),
                        "Token not '{:?}'. got {:?}",
                        Tk::Num("5".into(), 2),
                        first_token
                    );
                    assert_eq!(stmt.to_literal(), "5".to_string());
                }
                _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
            }
        }
    }
}
