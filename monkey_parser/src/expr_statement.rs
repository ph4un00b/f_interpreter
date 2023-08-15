use crate::{
    ast::P,
    ast_expression::Expr,
    ast_statements::Statement,
    parser::{Assertions, Parsing},
};

pub struct ExprStatement;

impl ExprStatement {
    pub(crate) fn literal(
        first_token: &crate::scanner::Tk,
        _expr: &crate::ast_expression::Expr,
    ) -> String {
        String::from(first_token)
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        _first_token: &crate::scanner::Tk,
        expr: &crate::ast_expression::Expr,
    ) -> Result<(), std::fmt::Error> {
        if let Expr::None = expr {
            write!(f, "")?;
        } else {
            write!(f, "{expr}")?;
        }
        Ok(())
    }

    pub(crate) fn parse(p: &mut crate::parser::Parser) -> Option<crate::ast_statements::Statement> {
        let first_token = p.current_token.clone();
        let expr = p.parse_expression(P::Lowest);
        if p.optional_semi() {
            p.next_token();
        }
        if let Some(expr) = expr {
            Some(Statement::Expr { first_token, expr })
        } else {
            Some(Statement::Expr {
                first_token,
                expr: Expr::None,
            })
        }
    }
}
