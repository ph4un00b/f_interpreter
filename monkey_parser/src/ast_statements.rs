use crate::{
    ast::{FnType, ToLiteral},
    ast_expression::Expr,
    bind_statement::LetStatement,
    expr_statement::ExprStatement,
    parser::Parser,
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
        kind: FnType,
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
            Statement::Bind {
                token,
                identifier,
                initializer,
            } => LetStatement::display(f, token, identifier, initializer),
            Statement::Return { token, value } => ReturnStatement::display(f, token, value),
            Statement::None(_) => todo!(),
            Statement::Expr { first_token, expr } => ExprStatement::display(f, first_token, expr),
            Statement::Print(_) => todo!(),
            Statement::Block(_) => todo!(),
            Statement::Row {
                name: _,
                super_expr: _,
                columns: _,
            } => {
                todo!()
            }
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
        }
    }
}

impl Statement {
    pub fn parse(p: &mut Parser) -> Option<Statement> {
        match &p.current_token {
            Tk::Let => LetStatement::parse(p),
            Tk::Return => ReturnStatement::parse(p),
            Tk::Assign
            | Tk::Plus
            | Tk::Sub
            | Tk::Bang
            | Tk::Mul
            | Tk::Div
            | Tk::LT
            | Tk::GT
            | Tk::LP
            | Tk::RP
            | Tk::LB
            | Tk::RB
            | Tk::Comma
            | Tk::Semi
            | Tk::End
            | Tk::String(_, _)
            | Tk::Ident(_, _)
            | Tk::Num(_, _)
            | Tk::None
            | Tk::Func
            | Tk::If
            | Tk::Else
            | Tk::True
            | Tk::False
            | Tk::EQ
            | Tk::NotEq
            | Tk::Comment
            | Tk::EMOJI4(_, _)
            | Tk::EMOJI3(_, _)
            | Tk::EMOJI2(_, _)
            | Tk::EMOJI1(_, _) => None,
        }
    }
}
