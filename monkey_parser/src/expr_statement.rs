use crate::ast_expression::Expr;

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
            write!(f, "{expr};")?;
        }
        Ok(())
    }
}
