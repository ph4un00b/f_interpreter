use crate::{
    ast::P,
    ast_expression::Expr,
    parser::{Assertions, Parsing},
    scanner::Tk,
};

pub struct GroupExpr;
impl GroupExpr {
    pub(crate) fn parse(
        p: &mut crate::parser::MonkeyParser,
    ) -> Option<crate::ast_expression::Expr> {
        p.next_token();
        let maybe_expr = p.parse_expression(P::Lowest);
        if !p.expect_peek(Tk::CloseParen) {
            return None;
        }
        maybe_expr.map(|expr| Expr::Grouping(Box::new(expr)))
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        expr: &Expr,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "#{expr}")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parser_test::parse_program;

    #[test]
    fn test_grouped_precedence() {
        let tests = [
            ("1 + (2 + 3) + 4", "((1 + #(2 + 3)) + 4)"),
            ("(5 + 5) * 2", "(#(5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / #(5 + 5))"),
            ("-(5 + 5)", "(-#(5 + 5))"),
            ("!(true == true)", "(!#(true == true))"),
        ];

        for (input, expected) in tests {
            let program = parse_program(input);
            assert_eq!(program.to_string(), expected);
        }
    }
}
