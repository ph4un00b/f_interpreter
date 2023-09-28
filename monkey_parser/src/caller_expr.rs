use crate::{
    ast::P,
    ast_expression::Expr,
    parser::{Assertions, Parsing},
    scanner::Tk,
};

pub struct CallerExpr;
impl CallerExpr {
    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        _token: &crate::scanner::Tk,
        callee: &Expr,
        arguments: &[Expr],
    ) -> Result<(), std::fmt::Error> {
        let mut args = vec![];
        for item in arguments {
            args.push(item.to_string());
        }
        write!(f, "#{callee}")?;
        write!(f, " ")?;
        write!(f, "{}", args.join(" "))?;
        write!(f, "")?;
        Ok(())
    }

    pub(crate) fn parse(p: &mut crate::parser::MonkeyParser, function: Expr) -> Option<Expr> {
        Some(Expr::Caller {
            token: p.current_token.to_owned(),
            callee: Box::new(function),
            arguments: parse_arguments(p),
        })
    }

    pub(crate) fn name() -> String {
        String::from("call")
    }

    pub(crate) fn literal(token: &Tk) -> String {
        token.to_string()
    }
}

fn parse_arguments(p: &mut crate::parser::MonkeyParser) -> Vec<Expr> {
    let mut args = vec![];
    if p.peek_token == Tk::CloseParen {
        p.next_token();
        return args;
    }
    p.next_token();
    append_argument(p, &mut args);
    while p.peek_token == Tk::Comma {
        p.next_token();
        p.next_token();
        append_argument(p, &mut args);
    }
    if !p.expect_peek(Tk::CloseParen) {
        return args;
    }
    args
}

fn append_argument(p: &mut crate::parser::MonkeyParser, args: &mut Vec<Expr>) {
    if let Some(expr) = p.parse_expression(P::Lowest) {
        args.push(expr);
    };
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Name, ToLiteral, V},
        ast_expression::Expr,
        ast_statements::Statement,
        parser_test::{assert_identifier, assert_literal_boxed_expression, parse_program},
    };

    #[test]
    fn test_call_parsing() {
        let input = r#"
            add(1, 2 * 3, 4 + 5);
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
            assert_eq!(if_expr.name(), "call");
            assert_eq!(if_expr.to_literal(), "(");
            assert_eq!(if_expr.to_string(), "#add 1 (2 * 3) (4 + 5)");
            assert_call_expr(stmt);
        }
    }

    fn assert_call_expr(stmt: Statement) {
        println!("> {stmt}");
        let (fn_callee, fn_args) = match stmt.clone() {
            Statement::Expr {
                first_token: _,
                expr:
                    Expr::Caller {
                        token: _,
                        callee,
                        arguments,
                    },
            } => (*callee, arguments),
            _ => unreachable!("not *ast.Statement::Expr::Func. got {stmt:?}"),
        };
        assert_identifier(fn_callee, "id", "add", "add");
        //? arguments
        assert_eq!(
            fn_args.len(),
            3,
            "function literal parameters wrong. want 3, got {}",
            fn_args.len()
        );

        let expr = fn_args[0].clone();
        let expected_value = V::I64(1);
        assert_literal_expr(expr, expected_value);
        let expr = fn_args[1].clone();
        let expected_left = V::I64(2);
        let expected_op = "*";
        let expected_right = V::I64(3);
        assert_infix_expr(expr, expected_left, expected_op, expected_right);
        let expr = fn_args[1].clone();
        let expected_left = V::I64(2);
        let expected_op = "*";
        let expected_right = V::I64(3);
        assert_infix_expr(expr, expected_left, expected_op, expected_right);
    }

    fn assert_infix_expr(expr: Expr, expected_left: V, expected_op: &str, expected_right: V) {
        if let Expr::Binary { left, op, right } = expr {
            assert_literal_boxed_expression(left, expected_left);
            assert_eq!(
                &op.to_string(),
                expected_op,
                "exp.Operator is not '{expected_op}'. got {op}",
            );
            assert_literal_boxed_expression(right, expected_right);
        }
    }

    fn assert_literal_expr(expr: Expr, expected_value: V) {
        let literal = expr.to_literal();
        let val = match expr {
            Expr::Literal { token: _, value } => value,
            _ => unreachable!("not *ast.LiteralExpression. got {expr}"),
        };
        assert_eq!(val, expected_value, "Value not {expected_value}. got {val}");
        assert_eq!(
            literal,
            expected_value.to_string(),
            "TokenLiteral not {expected_value}. got {literal}"
        );
    }
}
