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
        // TODO: We're skipping the expressions until we encounter a semicolon
        while p.current_token_isnt_semi() {
            p.next_token();
        }
        Some(Statement::Bind {
            token,
            identifier: name,
            initializer: Expr::None,
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
    use crate::{lexer::Lexer, parser::Errors};

    use super::*;

    #[test]
    fn test_let_statement() {
        let statements = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;
        let lex = Lexer::new(statements.into());
        let mut p = Parser::new(lex);
        let program = p.parse_program();
        for err in p.errors() {
            println!("🎈 {err}");
        }
        assert_eq!(p.errors().len(), 0);
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
        // let (statements, environment) = test_setup(tokens, vec![]);
        // let _ = test_run(environment, statements);
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
