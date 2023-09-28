use crate::{
    ast::FnKind,
    ast_expression::Expr,
    ast_statements::Statement,
    block_stmt::BlockStatement,
    parser::{Assertions, Parsing},
    scanner::Tk,
};

pub struct FunctionExpr;
impl FunctionExpr {
    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        token: &Tk,
        _kind: &crate::ast::FnKind,
        params: &[Tk],
        body: &Statement,
    ) -> Result<(), std::fmt::Error> {
        let mut parameters = vec![];
        for param in params {
            parameters.push(param.to_string());
        }
        write!(f, "{token}")?;
        write!(f, "(")?;
        write!(f, "{}", parameters.join(","))?;
        write!(f, ") ")?;
        write!(f, "{body}")?;
        Ok(())
    }

    pub(crate) fn parse(
        p: &mut crate::parser::MonkeyParser,
    ) -> Option<crate::ast_expression::Expr> {
        let token = p.current_token.to_owned();
        if !p.expect_peek(Tk::OpenParen) {
            return None;
        }
        let params = Self::parse_params(p);
        if !p.expect_peek(Tk::CreateBlock) {
            return None;
        }
        BlockStatement::parse(p).map(|block| Expr::Func {
            kind: FnKind::Literal,
            token,
            params,
            body: Box::new(block),
        })
    }

    fn parse_params(p: &mut crate::parser::MonkeyParser) -> Vec<Tk> {
        let mut identifiers = vec![];
        if p.peek_token == Tk::CloseParen {
            p.next_token();
            return identifiers;
        }
        p.next_token();
        identifiers.push(p.current_token.clone());
        while p.peek_token == Tk::Comma {
            p.next_token();
            p.next_token();
            identifiers.push(p.current_token.clone());
        }
        if !p.expect_peek(Tk::CloseParen) {
            return vec![];
        }
        identifiers
    }

    pub(crate) fn name() -> String {
        String::from("func")
    }

    pub(crate) fn literal(token: &Tk) -> String {
        token.to_string()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Name, ToLiteral},
        ast_expression::Expr,
        ast_statements::Statement,
        parser_test::{assert_id_expression, parse_program},
    };

    #[test]
    fn test_func_parameters_parsing() {
        let tests = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x,y,z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected_params) in tests {
            let program = parse_program(input);
            let stmt = &program.statements[0];
            let parameters = match stmt {
                Statement::Expr {
                    first_token: _,
                    expr:
                        Expr::Func {
                            kind: _,
                            token: _,
                            params,
                            body: _,
                        },
                } => params,
                _ => unreachable!("not *ast.Statement::Expr::Func. got {stmt:?}"),
            };
            assert_eq!(
                parameters.len(),
                expected_params.len(),
                "program has not enough statements. got {}",
                expected_params.len(),
            );
            for (i, param) in expected_params.iter().enumerate() {
                assert_eq!(&parameters[i].to_string().as_str(), param);
            }
        }
    }

    #[test]
    fn test_function_parsing() {
        let input = r#"
            fn(x, y) { x + y; }
        "#;
        let program = parse_program(input);
        println!("{program:?}");
        assert_eq!(
            program.len(),
            1,
            "program has not enough statements. got {}",
            program.len()
        );

        for stmt in program {
            let if_expr = match stmt.clone() {
                Statement::Expr {
                    first_token: _,
                    expr,
                } => expr,
                _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
            };
            assert_eq!(if_expr.name(), "func");
            assert_eq!(if_expr.to_literal(), "fn");
            assert_eq!(if_expr.to_string(), "fn(x,y) { (x + y) }");
            assert_func_expr(stmt);
        }
    }

    fn assert_func_expr(stmt: Statement) {
        println!("> {stmt}");
        let (fn_body, fn_params) = match stmt.clone() {
            Statement::Expr {
                first_token: _,
                expr:
                    Expr::Func {
                        kind: _,
                        token: _,
                        params,
                        body,
                    },
            } => (body, params),
            _ => unreachable!("not *ast.Statement::Expr::Func. got {stmt:?}"),
        };
        //? parameters
        assert_eq!(
            fn_params.len(),
            2,
            "function literal parameters wrong. want 2, got {}",
            fn_params.len()
        );
        assert_eq!(fn_params[0].to_string(), "x");
        assert_eq!(fn_params[1].to_string(), "y");
        let body_stmt = match *fn_body {
            Statement::Block(statements) => {
                assert_eq!(
                    statements.len(),
                    1,
                    "program has not enough statements. got {}",
                    statements.len(),
                );
                statements.first().unwrap().clone()
            }
            _ => unreachable!("not *ast.Statement::Block. got {stmt:?}"),
        };
        //? body
        let expected_left = "x";
        let expected_op = "+";
        let expected_right = "y";

        let binary_expr = match body_stmt {
            Statement::Expr {
                first_token: _,
                expr,
            } => expr,
            _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
        };
        match binary_expr {
            Expr::Binary { left, op, right } => {
                assert_id_expression(left, expected_left);
                assert_eq!(
                    &op.to_string(),
                    expected_op,
                    "exp.Operator is not '{expected_op}'. got {op}",
                );
                assert_id_expression(right, expected_right);
            }
            _ => todo!(),
        }
    }
}
