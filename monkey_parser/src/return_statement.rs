use crate::{
    ast::P, ast_expression::Expr, ast_statements::Statement, parser::Parsing, scanner::Tk,
};

pub struct ReturnStatement;
impl ReturnStatement {
    pub(crate) fn literal(
        token: &crate::scanner::Tk,
        _value: &crate::ast_expression::Expr,
    ) -> String {
        String::from(token)
    }

    pub(crate) fn parse(p: &mut crate::parser::Parser) -> Option<crate::ast_statements::Statement> {
        let token = p.current_token.clone();
        p.next_token();
        let maybe_return = p.parse_expression(P::Lowest);
        if p.peek_token == Tk::Semi {
            p.next_token();
        }
        maybe_return.map(|value| Statement::Return { token, value })
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        token: &crate::scanner::Tk,
        value: &Expr,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{token}")?;
        if let Expr::None = value {
            write!(f, ";")?;
        } else {
            write!(f, "{value};")?;
        };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast_expression::Expr,
        ast_statements::Statement,
        lexer::Lexer,
        parser::{Errors, Parser, Parsing},
        scanner::Tk,
    };

    #[test]
    fn test_return_statement() {
        let statements = r#"
            return 5;
            return 10;
            return 993322;
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
            "program.Statements does not contain 3 statements. got {}",
            program.len()
        );

        let test = ["x", "y", "foobar"];
        for (i, stmt) in program.enumerate() {
            match stmt {
                Statement::Return { token, value: _ } => {
                    assert_return(token, Expr::None, test[i]);
                }
                _ => unreachable!("not *ast.Statement::Return. got {stmt:?}"),
            }
        }
    }

    fn assert_return(token: Tk, _value: Expr, _test: &str) {
        assert_eq!(
            token,
            Tk::Return,
            "TokenLiteral not '{}'. got {:?}",
            Tk::Return,
            token
        );
    }
}
