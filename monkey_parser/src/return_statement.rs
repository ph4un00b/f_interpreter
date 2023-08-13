use crate::{
    ast_expression::Expr,
    ast_statements::Statement,
    parser::{Assertions, Parsing},
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
        // TODO: We're skipping the expressions until we encounter a semicolon
        while p.current_token_isnt_semi() {
            p.next_token();
        }
        Some(Statement::Return {
            token,
            value: Expr::None,
        })
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
        // let (statements, environment) = test_setup(tokens, vec![]);
        // let _ = test_run(environment, statements);
    }

    fn assert_return(token: Tk, _value: Expr, _test: &str) {
        assert_eq!(
            token,
            Tk::Return,
            "TokenLiteral not '{}'. got {:?}",
            Tk::Return,
            token
        );
        // assert_eq!(
        //     String::from(&identifier).as_str(),
        //     test,
        //     "letStmt.Name.Value not '{}'. got {:?}",
        //     test,
        //     String::from(&identifier).as_str()
        // );
    }
}
