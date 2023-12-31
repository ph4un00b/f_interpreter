use crate::{
    ast::{FnKind, ToLiteral},
    ast_expression::Expr,
    bind_statement::LetStatement,
    block_stmt::BlockStatement,
    expr_statement::ExprStatement,
    parser::MonkeyParser,
    return_statement::ReturnStatement,
    scanner::Tk,
};

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Statement {
    None(String),
    Expr {
        first_token: Tk,
        expr: Expr,
    },
    Print(Expr),
    Block(Vec<Statement>),
    Row {
        name: Tk,
        super_expr: Expr,
        columns: Vec<Statement>,
    },
    Return {
        token: Tk,
        value: Expr,
    },
    Func {
        kind: FnKind,
        name: Tk,
        parameters: Vec<Tk>,
        body: Vec<Statement>,
    },
    While {
        condition: Expr,
        body: Box<Statement>,
    },
    Cond {
        condition: Expr,
        then_statement: Box<Statement>,
        else_statement: Box<Statement>,
    },
    Bind {
        token: Tk,
        identifier: Tk,
        initializer: Expr,
    },
}

impl ToLiteral for Statement {
    fn to_literal(&self) -> String {
        match self {
            Statement::None(_) => todo!(),
            Statement::Expr { first_token, expr } => ExprStatement::literal(first_token, expr),
            Statement::Print(_) => todo!(),
            Statement::Block(_) => todo!(),
            Statement::Row {
                name: _,
                super_expr: _,
                columns: _,
            } => todo!(),
            Statement::Return { token, value } => ReturnStatement::literal(token, value),
            Statement::Func {
                kind: _,
                name: _,
                parameters: _,
                body: _,
            } => {
                todo!()
            }
            Statement::While {
                condition: _,
                body: _,
            } => todo!(),
            Statement::Cond {
                condition: _,
                then_statement: _,
                else_statement: _,
            } => todo!(),
            Statement::Bind {
                token,
                identifier,
                initializer,
            } => LetStatement::literal(token, identifier, initializer),
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None(_) => todo!(),
            Self::Bind {
                token,
                identifier,
                initializer,
            } => LetStatement::display(f, token, identifier, initializer),
            Self::Return { token, value } => ReturnStatement::display(f, token, value),
            Self::Expr { first_token, expr } => ExprStatement::display(f, first_token, expr),
            Self::Block(stmts) => BlockStatement::display(f, stmts),
            Self::Print(_) => todo!(),
            Self::Row {
                name: _,
                super_expr: _,
                columns: _,
            } => {
                todo!()
            }
            Self::Func {
                kind: _,
                name: _,
                parameters: _,
                body: _,
            } => {
                todo!()
            }
            Self::While {
                condition: _,
                body: _,
            } => todo!(),
            Self::Cond {
                condition: _,
                then_statement: _,
                else_statement: _,
            } => todo!(),
        }
    }
}

impl Statement {
    pub fn parse(p: &mut MonkeyParser) -> Option<Statement> {
        match &p.current_token {
            Tk::Let => LetStatement::parse(p),
            Tk::Return => ReturnStatement::parse(p),
            //todo is it possible to avoid repetition in #parse_expression❓
            //todo a macro❓
            Tk::OpenParen
            | Tk::Func
            | Tk::If
            | Tk::False
            | Tk::True
            | Tk::Sub
            | Tk::Bang
            | Tk::Ident(_, _)
            | Tk::Num(_, _) => ExprStatement::parse(p),
            Tk::Assign
            | Tk::Plus
            | Tk::Mul
            | Tk::Div
            | Tk::LT
            | Tk::GT
            | Tk::CloseParen
            | Tk::CreateBlock
            | Tk::EndBlock
            | Tk::Comma
            | Tk::Semi
            | Tk::End
            | Tk::String(_, _)
            | Tk::Else
            | Tk::EQ
            | Tk::NotEq
            | Tk::Comment
            | Tk::EMOJI4(_, _)
            | Tk::EMOJI3(_, _)
            | Tk::EMOJI2(_, _)
            | Tk::EMOJI1(_, _)
            | Tk::None => None,
        }
    }
}
