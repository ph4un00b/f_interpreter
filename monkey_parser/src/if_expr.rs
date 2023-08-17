use crate::{
    ast::P,
    ast_expression::Expr,
    ast_statements::Statement,
    block_stmt::BlockStatement,
    parser::{Assertions, Parsing},
    scanner::Tk,
};

pub struct IfExpr;
impl IfExpr {
    pub(crate) fn name() -> String {
        String::from("if")
    }

    pub(crate) fn literal(token: &Tk) -> String {
        token.to_string()
    }

    pub(crate) fn parse(p: &mut crate::parser::Parser) -> Option<Expr> {
        let token = p.current_token.to_owned();
        if !p.expect_peek(Tk::OpenParen) {
            return None;
        }
        p.next_token();
        let maybe_condition = p.parse_expression(P::Lowest);
        if !p.expect_peek(Tk::CloseParen) {
            return None;
        }
        if !p.expect_peek(Tk::CreateBlock) {
            return None;
        }
        let maybe_consequence = BlockStatement::parse(p);
        let maybe_alternative = if p.peek_token == Tk::Else {
            p.next_token();
            if !p.expect_peek(Tk::CreateBlock) {
                return None;
            };
            BlockStatement::parse(p)
        } else {
            None
        };
        Some(Expr::If {
            token,
            // todo: test none unwraps
            condition: Box::new(maybe_condition.unwrap()),
            then: Box::new(maybe_consequence.unwrap()),
            alternative: maybe_alternative.map(Box::new),
        })
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        condition: &Expr,
        then: &Statement,
        alternative: Option<&Statement>,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "if ")?;
        write!(f, "{condition}")?;
        write!(f, " ")?;
        write!(f, "{then}")?;
        if let Some(else_branch) = alternative {
            write!(f, " else {else_branch}")?;
        } else {
            write!(f, "")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Name, ToLiteral},
        ast_expression::Expr,
        ast_statements::Statement,
        parser_test::{assert_id_expression, assert_identifier, parse_program},
    };

    #[test]
    fn test_if_parsing() {
        let input = r#"
            if (x < y) { x }
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
            assert_eq!(if_expr.name(), "if");
            assert_eq!(if_expr.to_literal(), "if");
            assert_eq!(if_expr.to_string(), "if (x < y) { x }");
            assert_if_expr(stmt);
        }
    }

    #[test]
    fn test_if_else_parsing() {
        let input = r#"
            if (x < y) { x } else { y }
        "#;
        let program = parse_program(input);
        println!("{program:?}");
        println!("{program}");
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
            assert_eq!(if_expr.name(), "if");
            assert_eq!(if_expr.to_literal(), "if");
            assert_eq!(if_expr.to_string(), "if (x < y) { x } else { y }");
            assert_if_else_expr(stmt);
        }
    }

    fn assert_if_expr(stmt: Statement) {
        println!("> {stmt}");
        let (condition, then) = match stmt.clone() {
            Statement::Expr {
                first_token: _,
                expr:
                    Expr::If {
                        token: _,
                        condition,
                        then,
                        alternative: None,
                    },
            } => (condition, then),
            _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
        };

        //? condition
        let expected_left = "x";
        let expected_op = "<";
        let expected_right = "y";

        match *condition {
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
        //? if-block
        let consequence = match *then {
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
        //? then
        match consequence {
            Statement::Expr {
                first_token: _,
                expr,
            } => {
                assert_identifier(expr, "id", "x", "x");
            }
            _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
        }
    }

    fn assert_if_else_expr(stmt: Statement) {
        println!("> {stmt}");
        let (condition, then, alternative) = match stmt.clone() {
            Statement::Expr {
                first_token: _,
                expr:
                    Expr::If {
                        token: _,
                        condition,
                        then,
                        alternative: Some(else_branch),
                    },
            } => (condition, then, else_branch),
            _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
        };
        //? condition
        let expected_left = "x";
        let expected_op = "<";
        let expected_right = "y";

        match *condition {
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
        //? if-block
        let consequence = match *then {
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
        //? then
        match consequence {
            Statement::Expr {
                first_token: _,
                expr,
            } => {
                assert_identifier(expr, "id", "x", "x");
            }
            _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
        }
        //? else-block
        let else_consequence = match *alternative {
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
        //? alternative
        match else_consequence {
            Statement::Expr {
                first_token: _,
                expr,
            } => {
                assert_identifier(expr, "id", "y", "y");
            }
            _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
        }
    }
}
