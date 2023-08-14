use crate::{
    ast::P,
    ast_expression::Expr,
    parser::{Errors, Parsing},
    scanner::Tk,
};

pub struct PrefixExpr;
impl PrefixExpr {
    pub(crate) fn literal(op: &crate::scanner::Tk) -> String {
        op.to_string()
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        op: &crate::scanner::Tk,
        right: &Expr,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "(")?;
        write!(f, "{op}")?;
        write!(f, "{right}")?;
        write!(f, ")")?;
        Ok(())
    }

    pub(crate) fn parse(p: &mut crate::parser::Parser) -> Option<Expr> {
        let op = p.current_token.to_owned();
        if let Tk::Sub | Tk::Bang = op {
            p.next_token();
            p.parse_expression(P::Prefix).map(|expr| Expr::Unary {
                op,
                right: Box::new(expr),
            })
        } else {
            p.append_error(format!("no prefix parse function for {} found", op));
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{ToLiteral, V},
        ast_expression::Expr,
        ast_statements::Statement,
        lexer::Lexer,
        parser::{Errors, Parser, Parsing},
    };

    #[test]
    fn test_prefix_expressions() {
        let tests = vec![
            ("!5", "!".to_string(), 5i64),
            ("-15", "-".to_string(), 15i64),
        ];

        for (_index, test) in tests.iter().enumerate() {
            let (test_input, expected_op, expected_value) = test;
            let program = setup(test_input);
            assert_eq!(
                program.len(),
                1,
                "program has not enough statements. got {}",
                program.len()
            );

            for (_i, stmt) in program.enumerate() {
                println!("> {stmt:?}");
                let (op, expr) = match stmt {
                    Statement::Expr {
                        first_token: _,
                        expr,
                    } => match expr {
                        Expr::Unary { op, right } => (op, right),
                        _ => unreachable!("stmt is not ast.PrefixExpression. got {expr:?}"),
                    },
                    _ => unreachable!("not *ast.Statement::Expr. got {stmt:?}"),
                };
                assert_eq!(
                    &op.to_string(),
                    expected_op,
                    "exp.Operator is not '{expected_op}'. got {op}",
                );
                let literal = expr.to_literal();
                let val = match *expr {
                    Expr::Literal {
                        token: _,
                        value: V::I64(val),
                    } => val,
                    _ => unreachable!("not *ast.IntegerLiteral. got {expr}"),
                };
                assert_eq!(
                    &val, expected_value,
                    "Value not {expected_value}. got {val}"
                );
                assert_eq!(
                    literal,
                    expected_value.to_string(),
                    "TokenLiteral not {expected_value}. got {literal}"
                );
            }
        }
    }

    fn setup(input: &str) -> crate::program_node::Program {
        let lex = Lexer::new(input.into());
        let mut p = Parser::new(lex);
        let program = p.parse_program();
        for err in p.errors() {
            println!("🎈 {err}");
        }
        assert_eq!(p.errors().len(), 0);
        program
    }
}
