use crate::{ast_statements::Statement, parser::Parsing, scanner::Tk};

pub struct BlockStatement;
impl BlockStatement {
    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        stmts: &[Statement],
    ) -> Result<(), std::fmt::Error> {
        for statement in stmts {
            write!(f, "{statement}")?;
        }
        Ok(())
    }

    pub(crate) fn parse(p: &mut crate::parser::Parser) -> Option<Statement> {
        // let token = p.current_token.to_owned();
        let mut statements = vec![];
        p.next_token();
        //todo: look for a refactor for double !=
        while p.current_token != Tk::EndBlock && p.current_token != Tk::End {
            let maybe_stmt = Statement::parse(p);
            if let Some(stmt) = maybe_stmt {
                statements.push(stmt);
            };
            p.next_token();
        }
        Some(Statement::Block(statements))
    }
}
