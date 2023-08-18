use crate::ast::P;
use crate::parser::{Assertions, Parsing};
use crate::{ast_expression::Expr, ast_statements::Statement, parser::Parser, scanner::Tk};
pub struct LetStatement;

impl LetStatement {
    pub fn literal(_token: &Tk, identifier: &Tk, _initializer: &Expr) -> String {
        String::from(identifier)
    }

    pub fn parse(p: &mut Parser) -> Option<Statement> {
        let token = p.current_token.clone();
        if !p.expect_peek_identifier() {
            return None;
        }
        let name = p.current_token.clone();
        if !p.expect_peek(Tk::Assign) {
            return None;
        }
        p.next_token();
        let maybe_initializer = p.parse_expression(P::Lowest);
        if p.peek_token == Tk::Semi {
            p.next_token();
        }
        maybe_initializer.map(|value| Statement::Bind {
            token,
            identifier: name,
            initializer: value,
        })
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        token: &Tk,
        identifier: &Tk,
        initializer: &Expr,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{token} ")?;
        write!(f, "{identifier}")?;
        write!(f, " = ")?;
        if let Expr::None = initializer {
            write!(f, ";")?;
        } else {
            write!(f, "{initializer};")?;
        };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parser_test::parse_program;

    use super::*;

    #[test]
    fn test_let_with_expr() {
        let tests = vec![("let foobar = y;", "foobar", "y")];

        for (input, expected_id, expected_expr) in tests {
            let program = parse_program(input);
            assert_eq!(
                program.len(),
                1,
                "program.Statements does not contain 3 statements. {}",
                program.len()
            );
            let stmt = &program.statements[0];
            println!("> {stmt:?}");
            match stmt {
                Statement::Bind {
                    token: _,
                    identifier,
                    initializer,
                } => {
                    assert_eq!(identifier.to_string().as_str(), expected_id);
                    assert_eq!(initializer.to_string().as_str(), expected_expr);
                }
                _ => unreachable!("not *ast.LetStatement"),
            }
        }
    }

    #[test]
    fn test_let_statement() {
        let statements = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;
        let program = parse_program(statements);
        assert_eq!(
            program.len(),
            3,
            "program.Statements does not contain 3 statements. {}",
            program.len()
        );

        let test = ["x", "y", "foobar"];
        for (i, stmt) in program.enumerate() {
            match stmt {
                Statement::Bind {
                    token,
                    identifier,
                    initializer: _,
                } => {
                    assert_id(token, identifier, test[i]);
                }
                _ => unreachable!("not *ast.LetStatement. got {stmt:?}"),
            }
        }
    }

    fn assert_id(token: Tk, identifier: Tk, test: &str) {
        assert_eq!(token, Tk::Let, "TokenLiteral not 'let'. got {:?}", token);
        assert_eq!(
            String::from(&identifier).as_str(),
            test,
            "letStmt.Name.Value not '{}'. got {:?}",
            test,
            String::from(&identifier).as_str()
        );
    }
}
